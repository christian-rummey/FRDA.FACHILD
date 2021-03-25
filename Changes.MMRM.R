
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

# descriptive changes -----------------------------------------------------

est.aval <- dt.long %>% 
  filter ( avisitn != 1 ) %>%
  group_by( param ) %>% nest() %>% 
  mutate ( mod  = map( data, ~ lmer( cbl ~ bl + avisit + (1|sjid), data = ., control = .lme4.ctrl))) %>% 
  mutate ( emm  = map( mod , ~ emmeans::emmeans(., c('avisit')) )) %>%
  mutate ( coef = map( emm , ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>%
  # mutate ( emm  = map( mod , ~emmeans::emmeans(., pairwise ~ avisitn) )) %>% 
  # mutate ( coef = map( emm, ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>% 
  select(-data, -mod, -emm) %>% 
  unnest ( coef )

est.aval %<>% 
  bind_rows(dt.long %>% ungroup %>%  filter(avisit == 'BL') %>% select(avisit, param) %>% unique() %>%  mutate(estimate = 0)) %>% 
  mutate_at('avisit', factor, visitnames) %>% arrange(param, avisit) %>% 
  mutate( param = factor(param, (params.))) %>%  
  filter( avisit != '3y')

.long.plot.MMRM <- function (df, parids, title) {
  df %>% 
    filter(param %in% params.[parids]) %>% 
    ggplot()+geom_pointrange()+geom_line()+
    aes( avisit, estimate, ymin = conf.low, ymax = conf.high)+
    aes( group = param )+
    # geom_text ( aes(label = n , y = height), size = 3 )+
    geom_hline( yintercept = 0, linetype = 'dashed')+
    facet_wrap( ~ param, scale = 'free_y')+
    scale_x_discrete(labels = c('BL', '6m', '1y', '18m', '2y'))+
    .leg_ll+
    xlab('Visit')+
    ylab('Estimated Change (95%CI)')+
    ggtitle(title)
  }

# by subgroups -----------------------------------------------------------

dt.ss <- bind_rows(
  dt.long %>% filter(avisitn != 6) %>% mutate(group = amb.bl),
  dt.long %>% filter(avisitn != 6) %>% mutate(group = med.age),
  dt.long %>% filter(avisitn != 6) %>% mutate(group = med.FARS.E)
)

est.aval.sub <- dt.ss %>%
  filter ( avisitn != 1 ) %>%
  filter ( group != 'non-amb.') %>%
  group_by( param, group ) %>% nest() %>%
  mutate ( mod  = map( data, ~ lmer( cbl ~ bl + avisit + (1|sjid), data = ., control = .lme4.ctrl))) %>%
  mutate ( emm  = map( mod , ~ emmeans::emmeans(., c('avisit')) )) %>%
  mutate ( coef = map( emm , ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>%
  # mutate ( emm  = map( mod , ~emmeans::emmeans(., pairwise ~ avisitn) )) %>%
  # mutate ( coef = map( emm, ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>%
  select(-data, -mod, -emm) %>%
  unnest ( coef )

# est.aval.sub %>% saveRDS('DATA derived/coefficients.subgroups.rds')
# 
# est.aval.sub <- readRDS('DATA derived/coefficients.subgroups.rds')

est.aval.sub %<>% 
  bind_rows(dt.ss %>% ungroup %>%  filter(avisit == 'BL') %>% select(avisit, param, group) %>% unique() %>%  mutate(estimate = 0)) %>% 
  mutate_at('avisit', factor, visitnames) %>% arrange(param, avisit) %>% 
  mutate( param = factor(param, (params.))) %>%  
  filter( avisit != '3y')

.long.plot.MMRM.sub <- function (df, groups, parids, title) {
  
  .dodge <- position_dodge(width = 0.1)
  
  df %>% 
    filter(group %in% groups) %>% 
    # group_by(paramcd, param) %>% mutate(height = min(height)) %>% 
    filter(param %in% params.[parids]) %>% 
    ggplot()+geom_pointrange(position = .dodge)+geom_line(position = .dodge)+
    aes( avisit, estimate, ymin = conf.low, ymax=conf.high)+
    aes( shape = group )+ scale_shape_manual(values = c(21,19))+
    aes( group = group)+
    # geom_text ( aes(label = n , y = height), size = 3,position = position_dodge(width = 0.6) )+
    geom_hline( yintercept = 0, linetype = 'dashed')+
    facet_wrap( ~ param, scale = 'free_y')+
    scale_x_discrete(labels = c('BL', '6m', '1y', '18m', '2y'))+
    .leg_lr+
    xlab('Visit')+
    ylab('Estimated Change (95%CI)')+
    ggtitle(title)
  }


read_pptx ( '../Templates/CR.template.pptx' ) %>%
  add_slide ( layout = 'F', master = 'CR')  %>%
  ph_with   ( dml ( print ( 
    .long.plot.MMRM(est.aval, c(1,2,3,4,5),    'Extended Walking, T25FW & Peg Test - MMRM'),
    newpage = F ) ), location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide ( layout = 'F', master = 'CR')  %>%
  ph_with   ( dml ( print ( 
    .long.plot.MMRM(est.aval, c(8,9,10,11,12), 'Rating Scales and ADL - MMRM'),
    newpage = F ) ), location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide ( layout = 'F', master = 'CR')  %>%
  ph_with   ( dml ( print (
    .long.plot.MMRM.sub(est.aval.sub, c('ambulatory','non-amb.')         ,c(1,2,3,4,5),    'Extended Walking, T25FW & Peg Test - MMRM, by Ambulation'),
    newpage = F ) ), location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide ( layout = 'F', master = 'CR')  %>%
  ph_with   ( dml ( print (
    .long.plot.MMRM.sub(est.aval.sub, c('FARS.E < 22.8','FARS.E > 22.8') ,c(1,2,3,4,5),    'Extended Walking, T25FW & Peg Test - MMRM, by Median FARS E'),
    newpage = F ) ), location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide ( layout = 'F', master = 'CR')  %>%
  ph_with   ( dml ( print (
    .long.plot.MMRM.sub(est.aval.sub, c('age < 13.9','age > 13.9')       ,c(1,2,3,4,5),    'Extended Walking, T25FW & Peg Test - MMRM, by Median Age'),
    newpage = F ) ), location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide ( layout = 'F', master = 'CR')  %>%
  ph_with   ( dml ( print (
    .long.plot.MMRM.sub(est.aval.sub, c('ambulatory','non-amb.')         ,c(8,9,10,11,12), 'Rating Scales and ADL - MMRM, by Ambulation'),
    newpage = F ) ), location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide ( layout = 'F', master = 'CR')  %>%
  ph_with   ( dml ( print (
    .long.plot.MMRM.sub(est.aval.sub, c('FARS.E < 22.8','FARS.E > 22.8') ,c(8,9,10,11,12), 'Rating Scales and ADL - MMRM, by Median FARS E'),
    newpage = F ) ), location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide ( layout = 'F', master = 'CR')  %>%
  ph_with   ( dml ( print (
    .long.plot.MMRM.sub(est.aval.sub, c('age < 13.9','age > 13.9')       ,c(8,9,10,11,12), 'Rating Scales and ADL - MMRM, by Median Age'),
    newpage = F ) ), location = ph_location_type( type = "body" , id = 1) ) %>%
  print ( target = paste('Changes.MMRM.', gsub(":","-", Sys.time()), ".pptx", sep="") )

