
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

dt.long %<>%
  mutate(intervals = '6m') %>% 
  bind_rows(
    dt.long %>%
      filter(avisitn %in% c( 1, 3, 5 )) %>% 
      mutate(intervals = '1y') 
  )

with(dt.long, table (intervals, avisit))
     
# slope models -----------------------------------------------------

coefs <- dt.long %>% 
  ungroup %>% 
  filter( !(paramcd == 'w1m' & intervals == '1y') ) %>%
  filter( !(paramcd == 'w6m' & intervals == '1y') ) %>%
  group_by(intervals, paramcd, sjid) %>% filter(n()>1) %>% 
  group_by( intervals, param ) %>% 
  nest() %>%
  # filter(intervals == '6m') %>% 
  mutate ( mod.lmer  = map( data    , ~ lmer ( aval ~ bl + time. + (1 + time.|sjid), data = ., control = .lme4.ctrl))) %>% 
  mutate ( slp.lmer  = map( mod.lmer, ~ tidy ( .x, effects = "fixed", conf.int = T))) %>% 
  unnest ( slp.lmer ) %>% 
  filter ( term != '(Intercept)' ) %>% 
  filter ( term != 'bl' )

p1 <- coefs %>% 
  select  ( param, intervals, estimate, conf.low, conf.high, p.value ) %>% 
  arrange ( param, intervals ) %>% 
  filter  ( !(param %in% c('One Minute Walk (m)', '6 Minute Walk (m)')) ) %>% 
  ggplot  ()+
  geom_pointrange(position = position_dodge(width = 0.2))+
  aes(y = param, x = estimate)+
  aes(xmin = conf.low, xmax = conf.high)+
  aes(color = intervals)+scale_color_brewer(palette = 'Set1')+
  facet_wrap(~param, scales = 'free', nrow = 2)+
  geom_vline(xintercept = 0)+
  coord_flip()

t1 <- coefs %>% 
  filter  ( !(param %in% c('One Minute Walk (m)', '6 Minute Walk (m)','Lower Limbs (FARS.C)')) ) %>% 
  arrange(param, intervals) %>% mutate(`p-value` = sprintf('%.6f', p.value)) %>% 
  select  ( param, intervals, estimate, conf.low, conf.high, p.value ) 

t1 %>% .ct()
t1 %>% flextable

read_pptx ( '../Templates/CR.template.pptx' ) %>%
  add_slide ( layout = 'F', master = 'CR')  %>%
  ph_with   ( dml ( print ( 
    p1,
    newpage = F ) ), location = ph_location_type( type = "body" , id = 1) ) %>%
  # add_slide ( layout = 'F', master = 'CR')  %>%
  # ph_with   ( dml ( print ( 
  #   .long.plot.MMRM(est.aval, c(8,9,10,11,12), 'Rating Scales and ADL - MMRM'),
  #   newpage = F ) ), location = ph_location_type( type = "body" , id = 1) ) %>%
  print ( target = paste('Changes.Slopes.', gsub(":","-", Sys.time()), ".pptx", sep="") )


t1 %>% 
  filter(param %in% c('Timed up and Go (1/s)', 'Timed 25 Foot Walk (m/s)', '9 Hole Peg Test (1/min)','Berg Balance Scale')) %>% 
  ggplot()+geom_pointrange()+
  aes(x = estimate)+aes(shape = intervals)+
  aes(xmin = conf.low, xmax = conf.high)+
  aes(y = intervals)+
  facet_wrap(~param, ncol = 1, scales = 'free_x')+
  theme_minimal(base_size = 16)+
  geom_vline(xintercept = 0)+
  # geom_text(aes(label= paste ( 'p = ', sprintf('%.3g',p.value))), x = -0)+
  .box+
  