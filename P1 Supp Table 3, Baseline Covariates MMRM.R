
# setup ---------------------------------------------------------------

rm(list=ls())
source('project.settings.R')

require(lme4)
require(lmerTest)
require(optimx)
require(broom.mixed)
require(emmeans)
.lme4.ctrl <- lmerControl(optimizer ='optimx', calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))

# data --------------------------------------------------------------------

dm.   <- readRDS('DATA derived/dm.rds')
dt.   <- readRDS('DATA derived/dt.rds')

# Parameters --------------------------------------------------------------

dt. %<>% 
  filter( paramcd %in% c('mFARS','FARS.E','FARS.B','FARS.C') ) %>%
  droplevels()

# Subgroups ----------------------------------------------------------------

dt.s <- dt. %>% 
  left_join(dm. %>% select(sjid, bl.amb, itt, starts_with('subgroup'))) %>%
  filter( itt ) %>%
  mutate  ( s.group = 1) %>%
  filter( !is.na( s.group )) %>% 
  droplevels()

dt.s %>% ungroup %>% 
  select(study, sjid, s.group) %>% 
  unique %>% group_by(study) %>% mutate(N = n()) %>% 
  group_by(study, s.group, N) %>% 
  summarise(n = n()) %>% mutate(pct = round(100*n/N,0))

# Baseline Covariates, Full 3y Changes Model (Supplemental Table)----------

dt.s %>% 
  filter(paramcd %in% c('mFARS','FARS.E')) %>% 
  # .vnames %>% 
  filter(!is.na(bl)) %>% 
  filter(avisitn <= 3, avisitn > 0) %>% 
  left_join(dm. %>% select(study, sjid, sex, aoo, bl.age, gaa1)) %>% 
  mutate(gaa1 = gaa1/100) %>%
  group_by( study, paramcd ) %>% nest() %>%
  mutate ( mod  = map( data, ~ lmer( cbl ~ bl + avisit + bl.age  + aoo + gaa1 + (1|sjid), data = ., control = .lme4.ctrl))) %>%
  mutate ( coef = map( mod , ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>%
  select(-data, -mod) %>%
  unnest ( coef ) %>% 
  filter(!grepl('avisit', term)) %>% 
  .fixmod() %>% 
  filter(term != '(Intercept)')
  
.Last.value %>% 
  .ct

# Baseline Covariates, Slope Model -----------------------------------------

dt.tmp <- dt.s %>% 
  # filter(paramcd %in% c('mFARS','FARS.E')) %>% 
  .vnames %>% 
  filter   ( !is.na(bl)) %>% 
  # filter   ( avisitn <= 3, avisitn > 0) %>% # BL needs to stay for slope models
  filter   ( avisitn <= 3 ) %>% 
  left_join( dm. %>% select(study, sjid, sex, aoo, bl.age, gaa1)) %>% 
  mutate   ( gaa = gaa1/100) %>% 
  select   ( -age ) %>% 
  rename   ( age = bl.age) %>% 
  group_by( study, paramcd ) %>% 
  nest()

all.mods <- bind_rows(
  # dt.tmp %>% mutate( m = 'bl'           ) %>% mutate ( mod=map( data, ~ lmer( aval ~ bl +                   (time.|sjid), data = ., control = .lme4.ctrl))),
  # dt.tmp %>% mutate( m = 'bl, age'      ) %>% mutate ( mod=map( data, ~ lmer( aval ~ bl + age +             (time.|sjid), data = ., control = .lme4.ctrl))),
  # dt.tmp %>% mutate( m = 'bl, aoo'      ) %>% mutate ( mod=map( data, ~ lmer( aval ~ bl + aoo +             (time.|sjid), data = ., control = .lme4.ctrl))),
  # dt.tmp %>% mutate( m = 'bl, gaa'      ) %>% mutate ( mod=map( data, ~ lmer( aval ~ bl + gaa +             (time.|sjid), data = ., control = .lme4.ctrl))),
  # dt.tmp %>% mutate( m = 'bl, age + aoo') %>% mutate ( mod=map( data, ~ lmer( aval ~ bl + age + aoo +       (time.|sjid), data = ., control = .lme4.ctrl))),
  # dt.tmp %>% mutate( m = 'bl, age + gaa') %>% mutate ( mod=map( data, ~ lmer( aval ~ bl + age + gaa +       (time.|sjid), data = ., control = .lme4.ctrl))),
  # dt.tmp %>% mutate( m = 'bl, aoo + gaa') %>% mutate ( mod=map( data, ~ lmer( aval ~ bl + aoo + gaa +       (time.|sjid), data = ., control = .lme4.ctrl))),
  dt.tmp %>% mutate( m = 'bl, all'      ) %>% mutate ( mod=map( data, ~ lmer( aval ~ time. + bl + age + aoo + gaa + (time.|sjid), data = ., control = .lme4.ctrl)))
) %>% 
  group_by(m, paramcd)

coefs <- all.mods %>% 
  mutate ( coef = map( mod , ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>%
  unnest ( coef ) %>% 
  select ( study, paramcd, m, term, estimate, p.value, conf.low, conf.high)
  
coefs %>% 
  filter(!(term %in% c('bl','(Intercept)'))) %>% 
  arrange(paramcd) %>% 
  select(-conf.low, -conf.high, -estimate) %>% 
  .fixmod %>% 
  spread(term, p.value) %>% 
  .ct

# slope subgroup models ---------------------------------------------------
# slope models ------------------------------------------------------------

dt.tmp <- dt.s %>% 
  filter(paramcd %in% c('mFARS','FARS.E','FARS.B')) %>% 
  .vnames %>% 
  filter   ( !is.na(bl)) %>% 
  filter   ( avisitn <= 3, avisitn > 0) %>% 
  left_join( dm. %>% select(study, sjid, sex, aoo, bl.age, gaa1)) %>% 
  mutate   ( gaa = gaa1/100) %>% 
  select   ( -age ) %>% 
  rename   ( age = bl.age) %>% 
  # group_by( study, paramcd ) %>% nest() %>%
  group_by(        paramcd ) %>% 
  select(study, sjid, time., paramcd, bl, aval, itt, starts_with('subgroup') )

dt.all.subgroups <- bind_rows(
  dt.tmp %>% mutate(subgroup = 'all'                           ) %>% select(study, subgroup, everything()),
  dt.tmp %>% mutate(subgroup = paste('age'   , subgroup.age   )) %>% select(study, subgroup, everything()),
  dt.tmp %>% mutate(subgroup = paste('mFARS' , subgroup.mFARS )) %>% select(study, subgroup, everything()),
  dt.tmp %>% mutate(subgroup = paste('FARS.E', subgroup.FARS.E)) %>% select(study, subgroup, everything())
  ) %>% 
  filter(itt) %>% 
  filter(!is.na(subgroup))

dt.all.subgroups %<>% 
  filter(!grepl('NA', subgroup))
# %>% 
#   print(n=99)

dt.all.subgroups %<>% 
  group_by(study, sjid, subgroup, paramcd) %>% 
  filter(n()>1)

all.mods <- dt.all.subgroups %>% 
  group_by ( study, subgroup, paramcd ) %>% 
  nest() %>% 
  mutate ( mod  = map( data, ~ lmer( aval ~ bl + time. + (time.|sjid), data = ., control = .lme4.ctrl))) %>% 
  mutate ( coef = map( mod , ~ tidy ( .x, effects = 'fixed', conf.int = T )))

coefs <- all.mods %>% 
  unnest ( coef ) %>% 
  select ( study, subgroup, paramcd, term, estimate, p.value, conf.low, conf.high)

coefs %>% 
  mutate(subgroup = factor(subgroup, levels(factor(unique(coefs$subgroup)))[c(1, 2, 4, 5, 6, 7, 3)])) %>% 
  filter(term == 'time.') %>% 
  filter(!(term %in% c('bl','(Intercept)'))) %>% 
  arrange(paramcd) %>% 
  ggplot()+geom_pointrange(position = position_dodge(width = .25))+
  aes( x = estimate, xmin = conf.low, xmax = conf.high)+
  aes( y = subgroup )+
  aes( shape = study )+.ssmA+
  facet_grid(~paramcd)+
  geom_vline(xintercept = 0, type = 2)+
  .box+
  coord_flip()

