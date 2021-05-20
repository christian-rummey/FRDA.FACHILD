
# Setup ---------------------------------------------------------------

rm(list=ls())

require(lme4)
require(lmerTest)
require(optimx)
require(broom.mixed)
require(emmeans)
.lme4.ctrl <- lmerControl(optimizer ='optimx', calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))

require('labelled')
source('list.pars.R')

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

levels(dt.long$avisit) <- c('BL', '6m', '1y', '18m','2y','3y')

# descriptive changes -----------------------------------------------------

dt.s <- dt.long %>% 
  filter(avisitn != 6) %>% 
  group_by(paramcd, param, avisit, avisitn ) %>% 
  filter(!is.na(cbl)) %>% 
  summarise(n = n(), m = mean( cbl ), s = sd(cbl )) %>% 
  group_by(paramcd, param) %>% mutate(height = min(m-max(s)))

p.tm <- dt.s %>%
  filter(paramcd %in% pars.[c(1,2,3,4,5)]) %>% 
  # filter(is.na(avisit))
  ggplot()+geom_pointrange()+geom_line()+
  aes( avisit, m, ymin = m-s, ymax=m+s)+
  aes( group = param )+
  # aes( shape = grp )+scale_shape_manual(values = c(21,19))+
  # geom_text ( aes(label = sprintf('%.1f', estimate)), size = 1.5, nudge_y = -1) +
  geom_text ( aes(label = n , y = height), size = 3 )+
  geom_hline( yintercept = 0, linetype = 'dashed')+
  facet_wrap( ~ param, scale = 'free_y')+
  # coord_cartesian(ylim = c(0,5))+
  scale_x_discrete(labels = c('BL', '6m', '1y', '18m', '2y'))+
  theme_minimal(base_size = 18)+
  .leg_ll+
  xlab('Visit')+
  ylab('Mean Change (SD)')+
  ggtitle('Descriptive Changes, Extended Walking, T25FW & Peg Test')

p.tm

p.sc <- dt.s %>%
  filter(paramcd %in% pars.[c(8,9,10,11,12)]) %>% 
  # filter(is.na(avisit))
  ggplot()+geom_pointrange()+geom_line()+
  aes( avisit, m, ymin = m-s, ymax=m+s)+
  aes( group = param )+
  # aes( shape = grp )+scale_shape_manual(values = c(21,19))+
  # geom_text ( aes(label = sprintf('%.1f', estimate)), size = 1.5, nudge_y = -1) +
  geom_text ( aes(label = n , y = height), size = 3 )+
  geom_hline( yintercept = 0, linetype = 'dashed')+
  facet_wrap( ~ param, scale = 'free_y')+
  # coord_cartesian(ylim = c(0,5))+
  scale_x_discrete(labels = c('BL', '6m', '1y', '18m', '2y'))+
  theme_minimal(base_size = 18)+
  .leg_ll+
  xlab('Visit')+
  ylab('Mean Change (SD)')+
  ggtitle('Descriptive Changes, Rating Scales and ADL')

p.sc


dt.long %>% 
  group_by(paramcd, param, avisit, avisitn ) %>% 
  summarise(n(), m = mean(cbl,na.rm=T), s = sd(cbl,na.rm=T)) %>% 
  ggplot()+geom_pointrange()+geom_line()+
  aes( avisit, m, ymin = m-s, ymax=m+s)+
  aes( group = param )+
  # aes( shape = grp )+scale_shape_manual(values = c(21,19))+
  # geom_text ( aes(label = sprintf('%.1f', estimate)), size = 1.5, nudge_y = -1) +
  # geom_text ( aes(label = s) , y = .5)+
  geom_hline( yintercept = 0, linetype = 'dashed')+
  facet_wrap( ~ param, scale = 'free_y')+
  # coord_cartesian(ylim = c(0,5))+
  scale_x_discrete(labels = c('BL', '6m', '1y', '18m', '2y'))+
  theme_minimal(base_size = 18)+
  .leg_ll+
  xlab('Visit')+
  ylab('Estimated Change (95%CI)')+
  ggtitle('Descriptive Changes, Extended Walking, T25FW & Peg Test')


# cbl models --------------------------------------------------------------

est.aval <- dt.long %>% 
  filter ( avisitn != 1 ) %>%
  group_by( param ) %>% nest() %>% 
  mutate ( mod  = map( data, ~ lmer( cbl ~ avisit + (1|sjid), data = ., control = .lme4.ctrl))) %>% 
  mutate ( emm  = map( mod , ~ emmeans::emmeans(., c('avisit')) )) %>%
  mutate ( coef = map( emm , ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>%
  # mutate ( emm  = map( mod , ~emmeans::emmeans(., pairwise ~ avisitn) )) %>% 
  # mutate ( coef = map( emm, ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>% 
  unnest ( coef )

est.aval %>% 
  bind_rows(dt.long %>% ungroup %>%  filter(avisitn == 1) %>% select(avisitn, param) %>% unique() %>%  mutate(estimate = 0)) %>% 
  mutate( param = factor(param, (params.))) %>%  
  filter( param %in% c(params.[c(1, 2, 3, 4)])) %>%
  filter( avisit != '3y') %>% 
  ggplot()+geom_pointrange()+geom_line()+
  aes( avisit, estimate, ymin = conf.low, ymax=conf.high)+
  aes( group = param )+
  # aes( shape = grp )+scale_shape_manual(values = c(21,19))+
  # geom_text ( aes(label = sprintf('%.1f', estimate)), size = 1.5, nudge_y = -1) +
  # geom_text ( aes(label = s) , y = .5)+
  geom_hline( yintercept = 0, linetype = 'dashed')+
  facet_wrap( ~ param, scale = 'free_y')+
  # coord_cartesian(ylim = c(0,5))+
  scale_x_discrete(labels = c('BL', '6m', '1y', '18m', '2y'))+
  theme_minimal(base_size = 18)+
  .leg_ll+
  xlab('Visit')+
  ylab('Estimated Change (95%CI)')

# .sp('Estimated Changes, Walking')
# .sp('Estimated Changes, Rating Scales')
# .sp('Estimated Changes, mFARS Sub-Scores')

# change models all -----------------------------------------------------------

ft.x <- ft

estimates.all <- ft.x %>% 
  filter ( avisitn != 1 ) %>%
  group_by( param ) %>% nest() %>% 
  mutate ( mod  = map( data, ~ lmer( cbl ~ avisitn + (1|sjid), data = ., control = .lme4.ctrl))) %>% 
  mutate ( emm  = map( mod , ~ emmeans::emmeans(., c('avisitn')) )) %>%
  mutate ( coef = map( emm , ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>%
  # mutate ( emm  = map( mod , ~emmeans::emmeans(., pairwise ~ avisitn) )) %>% 
  # mutate ( coef = map( emm, ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>% 
  unnest ( coef )

# ft.x <- ft %>%
#   filter ( FARS.E > 12 ) %>% 
#   filter ( FARS.E < 30 )
# 
# estimates.decl <- ft.x %>% 
#   filter ( avisitn != 1 ) %>%
#   # group_by( param, sjid) %>% filter(max(avisitn)>3) %>% 
#   group_by( param ) %>% nest() %>% 
#   mutate ( mod  = map( data, ~ lmer( cbl ~ bl + avisitn + (1|sjid), data = ., control = .lme4.ctrl))) %>% 
#   mutate ( emm  = map( mod , ~ emmeans::emmeans(., c('avisitn')) )) %>%
#   mutate ( coef = map( emm , ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>%
#   # mutate ( emm  = map( mod , ~emmeans::emmeans(., pairwise ~ avisitn) )) %>% 
#   # mutate ( coef = map( emm, ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>% 
#   unnest ( coef )


means <- ft %>% filter(avisitn==1) %>% group_by(param, avisitn) %>% summarise( s = n(), estimate = mean(cbl, na.rm=T) ) %>% mutate(type = 'descr. means')

estimates.all %>%
  mutate(grp = 'all') %>% 
  # bind_rows( estimates.decl %>% mutate(grp = 'BL FARS.E 12-29'))  %>%
  bind_rows(
    means %>% mutate(grp = 'all'),
    # means %>% mutate(grp = 'BL FARS.E 12-29')
  ) %>%
  # filter(param == 'mFARS', avisitn %in% c(1,2,3)) %>% 
  ungroup %>% 
  filter( avisitn < 5 ) %>% 
  ggplot()+geom_pointrange()+geom_line()+
  aes( avisitn, estimate, ymin = conf.low, ymax=conf.high)+
  aes( group = grp )+
  # aes( shape = grp )+scale_shape_manual(values = c(21,19))+
  # geom_text ( aes(label = sprintf('%.1f', estimate)), size = 1.5, nudge_y = -1) +
  # geom_text ( aes(label = s) , y = .5)+
  geom_hline( yintercept = 0, linetype = 'dashed')+
  facet_wrap( ~ param, scale = 'free_y')+
  # coord_cartesian(ylim = c(0,5))+
  scale_x_discrete(labels = c('BL', '6m', '1y', '18m', '2y'))+
  theme_gray(base_size = 12)+
  .leg_ll+
  xlab('Visit')+
  ylab('estimated change (95%CI)')

# .sp('all', l = 1)

# # change models grouped by age.group -----------------------------------------------------------
# 
# ft.x <- ft %>%
#   filter ( FARS.E > 12 ) %>% 
#   filter ( FARS.E < 30 )
# 
# estimates.decl <- ft.x %>% 
#   filter ( avisitn != 1 ) %>%
#   # group_by( param, sjid) %>% filter(max(avisitn)>3) %>% 
#   group_by( param ) %>% nest() %>% 
#   mutate ( mod  = map( data, ~ lmer( cbl ~ bl + avisitn + age.groups + (1|sjid), data = ., control = .lme4.ctrl))) %>% 
#   mutate ( emm  = map( mod , ~ emmeans::emmeans(., c('avisitn', 'age.groups')) )) %>%
#   mutate ( coef = map( emm , ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>%
#   # mutate ( emm  = map( mod , ~emmeans::emmeans(., pairwise ~ avisitn) )) %>% 
#   # mutate ( coef = map( emm, ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>% 
#   unnest ( coef )
# 
# 
# means <- ft %>% filter(avisitn==1) %>% group_by(param, avisitn, age.groups) %>% summarise( s = n(), estimate = mean(cbl, na.rm=T) ) %>% mutate(type = 'descr. means')
# 
# estimates.decl %>%
#   mutate(grp = 'all') %>% 
#   # bind_rows( estimates.decl %>% mutate(grp = 'BL FARS.E 12-29'))  %>%
#   bind_rows(
#     means %>% mutate(grp = 'all'),
#     # means %>% mutate(grp = 'BL FARS.E 12-29')
#   ) %>%
#   # filter(param == 'mFARS', avisitn %in% c(1,2,3)) %>%
#   ungroup %>% 
#   ggplot()+geom_pointrange()+geom_line()+
#   aes( avisitn, estimate, ymin = conf.low, ymax=conf.high)+
#   aes( group = age.groups )+
#   aes( shape = age.groups )+scale_shape_manual(values = c(21,19))+
#   geom_hline( yintercept = 0, linetype = 'dashed')+
#   facet_wrap( ~ param, scale = 'free_y')+
#   scale_x_discrete(labels = c('BL', '', '1y', '', '2y'))+
#   theme_gray(base_size = 12)+
#   .leg_ll+
#   xlab('Visit')+
#   ylab('estimated change (95%CI)')
# 
# .sp('all', l = 1)
# 
# estimates %>% 
#   select(-c('data','mod','emm','df')) %>% 
#   pander::pander( split.table = Inf)
# 
# 
# # change models all -----------------------------------------------------------
# 
# ft.x <- ft
# 
# estimates.all <- ft.x %>% 
#   filter ( avisitn != 1 ) %>%
#   group_by( param, sjid) %>% filter(max(avisitn)>3) %>% 
#   group_by( param ) %>% nest() %>% 
#   mutate ( mod  = map( data, ~ lmer( cbl ~ avisitn + (1|sjid), data = ., control = .lme4.ctrl))) %>% 
#   mutate ( emm  = map( mod , ~ emmeans::emmeans(., c('avisitn')) )) %>%
#   mutate ( coef = map( emm , ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>%
#   # mutate ( emm  = map( mod , ~emmeans::emmeans(., pairwise ~ avisitn) )) %>% 
#   # mutate ( coef = map( emm, ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>% 
#   unnest ( coef )
# 
# ft.x <- ft %>%
#   filter ( FARS.E > 12 ) %>% 
#   filter ( FARS.E < 30 )
# 
# estimates.decl <- ft.x %>% 
#   filter ( avisitn != 1 ) %>%
#   group_by( param, sjid) %>% filter(max(avisitn)>3) %>% 
#   group_by( param ) %>% nest() %>% 
#   mutate ( mod  = map( data, ~ lmer( cbl ~ bl + avisitn + (1|sjid), data = ., control = .lme4.ctrl))) %>% 
#   mutate ( emm  = map( mod , ~ emmeans::emmeans(., c('avisitn')) )) %>%
#   mutate ( coef = map( emm , ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>%
#   # mutate ( emm  = map( mod , ~emmeans::emmeans(., pairwise ~ avisitn) )) %>% 
#   # mutate ( coef = map( emm, ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>% 
#   unnest ( coef )
# 
# 
# means <- ft %>% filter(avisitn==1) %>% group_by(param, avisitn) %>% summarise( s = n(), estimate = mean(cbl, na.rm=T) ) %>% mutate(type = 'descr. means')
# 
# estimates.all %>%
#   mutate(grp = 'all') %>% 
#   # bind_rows( estimates.decl %>% mutate(grp = 'BL FARS.E 12-29'))  %>% 
#   bind_rows(
#     means %>% mutate(grp = 'all'),
#     # means %>% mutate(grp = 'BL FARS.E 12-29')
#   ) %>%
#   # filter(param == 'mFARS', avisitn %in% c(1,2,3)) %>% 
#   ungroup %>% 
#   ggplot()+geom_pointrange()+geom_line()+
#   aes( avisitn, estimate, ymin = conf.low, ymax=conf.high)+
#   aes( group = grp )+
#   aes( shape = grp )+scale_shape_manual(values = c(21,19))+
#   # geom_text ( aes(label = sprintf('%.1f', estimate)), size = 1.5, nudge_y = -1) +
#   # geom_text ( aes(label = s) , y = .5)+
#   geom_hline( yintercept = 0, linetype = 'dashed')+
#   facet_wrap( ~ param, scale = 'free_y')+
#   # coord_cartesian(ylim = c(0,5))+
#   scale_x_discrete(labels = c('BL', '', '1y', '', '2y'))+
#   theme_gray(base_size = 12)+
#   .leg_ll+
#   xlab('Visit')+
#   ylab('estimated change (95%CI)')
# 
# .sp('all', l = 1)
# 
# estimates %>% 
#   select(-c('data','mod','emm','df')) %>% 
#   pander::pander( split.table = Inf)
# 
# 
# # change models decline phase ------------------------------------------------------
# # change models decline phase ------------------------------------------------------
# 
# ft.x <- ft %>%
#   filter ( FARS.E > 12 ) %>% 
#   filter ( FARS.E < 30 )
# 
# estimates <- ft.x %>% 
#   # filter (avisitn == 1)  %>%  
#   filter ( avisitn != 1 ) %>%
#   group_by( param ) %>% nest() %>% 
#   mutate ( mod  = map( data, ~ lmer( cbl ~ avisitn + (1|sjid), data = ., control = .lme4.ctrl))) %>% 
#   mutate ( emm  = map( mod , ~ emmeans::emmeans(., c('avisitn')) )) %>%
#   mutate ( coef = map( emm , ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>%
#   # mutate ( emm  = map( mod , ~emmeans::emmeans(., pairwise ~ avisitn) )) %>% 
#   # mutate ( coef = map( emm, ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>% 
#   unnest ( coef )
# 
# 
# estimates %>% 
#   select(-c('data','mod','emm','df')) %>% 
#   pander::pander( split.table = Inf)
# 
# means <- ft.x %>% group_by(param, avisitn) %>% summarise( s = n(),estimate = mean(cbl) ) %>% mutate(type = 'descr. means')
# 
# estimates %>% 
#   mutate(type = 'mmrm') %>% 
#   bind_rows(means) %>% 
#   ggplot()+geom_pointrange()+geom_line()+
#   aes( avisitn, estimate, ymin = conf.low, ymax=conf.high)+
#   aes( group = type )+
#   aes( shape = type )+scale_shape_manual(values = c(21,19))+
#   geom_text ( aes(label = s) , y = .5)+
#   geom_hline( yintercept = 0   )+
#   facet_wrap( ~ param, scale = 'free_y')+
#   # coord_cartesian(ylim = c(0,5))+
#   .leg_tl
# 
# .sp('bl less than 30 and g 12')
# 
# # change models decline phase by age group ------------------------------------------------------
# 
# ft.x <- ft %>%
#   filter ( bl > 12 ) %>% 
#   filter ( bl < 30 )
# 
# estimates <- ft.x %>% 
#   # filter (avisitn == 1)  %>%  
#   filter ( avisitn != 1 ) %>%
#   group_by( param) %>% nest() %>% 
#   mutate ( mod  = map( data, ~ lmer( cbl ~ avisitn + age.groups + (1|sjid), data = ., control = .lme4.ctrl))) %>% 
#   mutate ( emm  = map( mod , ~ emmeans::emmeans(., c('avisitn', 'age.groups')) )) %>%
#   mutate ( coef = map( emm , ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>%
#   # mutate ( emm  = map( mod , ~emmeans::emmeans(., pairwise ~ avisitn) )) %>% 
#   # mutate ( coef = map( emm, ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>% 
#   unnest ( coef )
# 
# 
# estimates %>% 
#   select(-c('data','mod','emm','df')) %>% 
#   pander::pander( split.table = Inf)
# 
# means <- ft.x %>% group_by(param, age.groups, avisitn) %>% summarise( s = n(), estimate = mean(cbl) ) %>% mutate(type = 'descr. means')
# 
# estimates %>% 
#   mutate(type = 'mmrm') %>% 
#   # bind_rows(means) %>% 
#   ggplot()+geom_pointrange()+geom_line()+
#   aes( avisitn, estimate, ymin = conf.low, ymax=conf.high)+
#   aes( group = paste(type, age.groups) )+
#   aes( shape = type )+scale_shape_manual(values = c(21,19))+
#   aes( color = age.groups )+
#   # geom_text ( aes(label = s) , y = .5)+
#   geom_hline( yintercept = 0   )+
#   facet_wrap( ~ param, scale = 'free_y')+
#   coord_cartesian(ylim = c(0,5))+
#   .leg_tl
# 
# .sp('bl less than 30 and g 12')
# 
# # . -----------------------------------------------------------------------
# 
# 
# mod <- ft %>% 
#   lmer(chg ~ base + avisitn*trtpg1 + (1|sjid), data = ., control = .lme4.ctrl)
# 
# pairs(mod)
# 
# mod %>% 
#   emmeans::emmeans(c('avisitn','trtpg1')) %>% pairs
#   # emmeans::emmeans(c(avisitn ~ trtpg1)) %>% 
#   emmeans::contrast(., 'Placebo')
#   
# coef(pairs(mod))
#   
# estimates <- ft %>% 
#   # filter ( avisitn==1 ) %>% #arrange(cbl) %>% filter(cbl!=0)
#   group_by( param ) %>% nest() %>% 
#   mutate ( mod  = map( data, ~lmer(chg ~ avisitn*trtpg1 + (1|sjid), data = ., control = .lme4.ctrl))) %>% 
#   mutate ( emm  = map( mod , ~emmeans::emmeans(., c('avisitn', 'trtpg1')) )) %>%
#   mutate ( coef = map( emm, ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>%
#   # mutate ( emm  = map( mod , ~emmeans::emmeans(., pairwise ~ avisitn) )) %>% 
#   # mutate ( coef = map( emm, ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>% 
#   unnest ( coef )
# 
# 
#   
# 
# 
