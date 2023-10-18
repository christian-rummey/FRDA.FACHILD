
# Setup ---------------------------------------------------------------

rm(list=ls())

require(lme4)
require(lmerTest)
require(optimx)
require(broom.mixed)
require(emmeans)
.lme4.ctrl <- lmerControl(optimizer ='optimx', calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))

require('labelled')
source('project.settings.R')

dt. <- readRDS('DATA derived/dt.rds')
dm. <- readRDS('DATA derived/dm.rds')

dt. %<>%
  left_join(
    dm. %>% 
      select(sjid, itt, bl.amb, starts_with('subgroup'))
    )

dt. %>% ungroup %>% 
  filter ( itt ) %>% 
  select ( study, sjid ) %>% unique %>% 
  select ( study ) %>% table

dt. %<>% 
  filter( itt ) %>% 
  mutate( param = factor(paramcd, levels = pars., labels = params.))

with(dt., table(avisitn, avisit))

dt. %<>% 
  filter ( avisitn <=3 ) %>% 
  mutate ( subgroup.all = 1) %>% 
  gather ( pop, value, starts_with('subgroup') ) %>% 
  mutate ( pop = paste( gsub('subgroup.','', pop) )) %>%
  select ( study, sjid, avisit, avisitn, pop, value, paramcd, param, bl, aval, cbl)

# descriptive changes -----------------------------------------------------

dt. %>% 
  filter(avisitn <=3 ) %>% 
  group_by(study, pop, value, paramcd, param, avisit, avisitn ) %>% 
  summarise(n = n(), m = mean( cbl ), s = sd( cbl ), SRM = m/s) %>% 
  .wds('DATA derived/SRMs')

dt. %<>% 
  # filter(pop == 'all') %>% 
  # filter(paramcd == 'FARS.B') %>% 
  droplevels()

# models ------------------------------------------------------------------

est.aval <- dt. %>% 
  filter ( avisitn <=3, avisitn > 0) %>% 
  group_by( study, pop, value, paramcd, param ) %>% nest() %>% 
  mutate ( mod  = map( data, ~ lmer( cbl ~ bl + avisit + (1|sjid), data = ., control = .lme4.ctrl))) %>% 
  mutate ( emm  = map( mod , ~ emmeans::emmeans(., c('avisit')) )) %>%
  mutate ( coef = map( emm , ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>%
  select(-data, -mod, -emm) %>% 
  unnest ( coef )

bl.zero.lines <- dt. %>%
  ungroup %>%
  filter( avisitn == 0 ) %>% 
  select( study, pop, value, paramcd, param ) %>% 
  unique() %>%  
  mutate(estimate = 0, avisit = 'BL' )

est.aval %<>% 
  bind_rows( bl.zero.lines ) %>% 
  mutate   ( avisit = factor( avisit, levels(dt.$avisit))) %>% 
  arrange  ( paramcd, avisit ) %>% 
  mutate   ( avisit = factor(avisit, c('BL','6m','1y','18m','2y','3y','3y+')) ) %>% 
  arrange  ( study, pop, value, paramcd, avisit)


est.aval %>% 
  left_join(dt. %>% ungroup %>% select(avisit, avisitn) %>% unique) %>% 
  .wds('DATA derived/all estimates')

