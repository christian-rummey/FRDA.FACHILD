
# setup ---------------------------------------------------------------

rm(list=ls())

require(lme4)
require(lmerTest)
require(optimx)
require(broom.mixed)
require(emmeans)
.lme4.ctrl <- lmerControl(optimizer ='optimx', calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))

require('labelled')
source('list.pars.R')

theme_set( 
  theme_minimal(base_size = 14)+
    theme(panel.grid.minor = element_blank())+
    .box
    )

dt.long   <- readRDS('DATA derived/dt.long.rds')

dt.bl     <- readRDS('DATA derived/dt.bl.rds')

dt.long %<>%
  filter(!grepl('.iu', paramcd)) %>% droplevels %>% 
  mutate( avisit = factor(avisitn)) %>% 
  left_join(
    readRDS('DATA derived/dt.bl.rds') %>% 
      rename(amb.bl = amb) %>%  
      select(sjid, amb.bl, med.age, med.FARS.E)
    ) %>%
  mutate(avisit = factor(avisitn)) %>% 
  select(study, sjid, avisitn, avisit, time., fpf, hpf, paramcd, param, aval, cbl, everything())

visitnames <- c('BL', '6m', '1y', '18m','2y','3y')

levels(dt.long$avisit) <- visitnames

# outcomes selection ------------------------------------------------------

# params <- params.[c(8)]
params <- params.

# Create & Save Models - Dataset ------------------------------------------

dt. <- dt.long %>%
  mutate( group = 'all') %>% 
  bind_rows( dt.long %>% mutate( group = amb.bl )) %>% 
  bind_rows( dt.long %>% mutate( group = med.age )) %>% 
  bind_rows( dt.long %>% mutate( group = med.FARS.E )) %>%
  left_join( dt.bl %>% select( sjid, bl.age, symp, gaa1 ) ) %>% 
  mutate(group = factor(group, c('all','ambulatory','non-amb.','age < 13.9','age > 13.9','FARS.E < 22.8','FARS.E > 22.8')))

dt. %<>% 
  left_join( dt.bl %>% select( sjid, bl.age, symp, gaa1 )  ) %>% 
  filter  ( param %in%  params) %>% 
  filter(!is.na(group)) %>% 
  arrange( param, group, avisitn)


# remove non-ambulatory walks ---------------------------------------------

dt. %<>% 
  filter(!(group == 'non-amb.' & paramcd %in% c('tug.i', 'w1m', 'w6m', 'w25.i')))

# Slope Models ----------------------------------------------------------

dt.mod.slope <- dt. %>% 
  group_by( param, group ) %>% nest() %>% 
  mutate  ( mod  = map( data, ~ lmer( aval ~ time. + bl + bl.age + (time.|sjid), data = ., control = .lme4.ctrl)))

dt.mod.slope %>% 
  saveRDS('DATA derived/dt.mod.slope.rds')

dt.coef.slope <- dt.mod.slope %>% 
  mutate ( coef = map( mod , ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>%
  unnest ( coef ) %>% 
  mutate_at('p.value', ~ sprintf('%.5f', .)) %>% 
  select( param, term, estimate, conf.low, conf.high, p.value )

dt.coef.slope %>% 
  saveRDS('DATA derived/dt.coef.slope.rds')

# AVAL Models ----------------------------------------------------------
# This is not fully thought through yet....

dt.mod.aval <- dt. %>% 
  filter(param %in%  params) %>% 
  group_by( param, group ) %>% nest() %>% 
  mutate ( mod  = map( data, ~ lmer( aval ~ bl + avisit + (1|sjid), data = ., control = .lme4.ctrl)))

dt.mod.aval %>% 
  saveRDS('DATA derived/dt.mod.aval.rds')

dt.coef.aval <- dt.mod.aval %>% 
  mutate ( coef = map( mod , ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>%
  unnest ( coef ) %>% 
  mutate_at('p.value', ~ sprintf('%.5f', .)) %>% 
  select( param, term, estimate, conf.low, conf.high, p.value )

dt.coef.aval %>% 
  saveRDS('DATA derived/dt.coef.aval.rds')

# chg-models --------------------------------------------------------------

dt.mod.chg <- dt. %>%  
  filter  ( avisitn != 1 ) %>%
  group_by( param, group ) %>% nest() %>% 
  mutate  ( mod  = map( data, ~ lmer( cbl ~ bl + avisit + (1|sjid), data = ., control = .lme4.ctrl)))

dt.mod.chg %>% 
  saveRDS('DATA derived/dt.mod.chg.rds')

dt.coef.chg <- dt.mod.chg %>% 
  mutate( coefs = map (mod, ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>% 
  unnest( coefs )

dt.coef.chg %>% 
  saveRDS('DATA derived/dt.coef.chg.rds')

dt.est.chg <- dt.mod.chg %>% 
  mutate ( emm  = map( mod , ~ emmeans::emmeans(., c('avisit')) )) %>%
  mutate ( coef = map( emm , ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>%
  select(-data, -mod, -emm) %>% 
  unnest ( coef )

dt.est.chg %<>% 
  bind_rows(dt. %>% ungroup %>%  filter(avisit == 'BL') %>% select(avisit, param, group) %>% unique() %>%  mutate(estimate = 0)) %>% 
  mutate_at('avisit', factor, visitnames) %>% arrange(param, group, avisit) %>% 
  mutate( param = factor(param, (params.))) %>%  
  filter( avisit != '3y')

dt.est.chg %>% 
  saveRDS('DATA derived/dt.est.chg.rds')

