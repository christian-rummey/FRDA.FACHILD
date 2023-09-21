
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

# 1    mFARS # 2   FARS.E # 3   FARS.B
# 4   FARS.C # 5  FARS.Am # 6    w25.i
# 7    hpt.i # 8    tug.i # 9      w1m 
# 10     w6m # 11     bbs # 12     ADL
# 13   TOT.C # 14   TOT.P # 15    PF.C
# 16    PF.P # 17    EF.C # 18    EF.P
# 19    SO.C # 20    SO.P # 21    SC.C
# 22    SC.P

id <- 1

par.   <- pars.[c(1)]

# slide title -------------------------------------------------------------

# title <- 'FARS by Study'
# title <- 'FARS by ambulation Status'
# title <- 'FARS by Median Age'
# title <- 'FARS by Median mFARS'
# title <- 'FARS by Median FARS E'
# title <- 'FARS by ambulation Status'
title <- paste0('FACHILD Results, ', params.[id] ) 

# Parameters --------------------------------------------------------------

dt. %<>% 
  # filter( study == 'FACHILD') %>%
  filter( paramcd == par. ) %>%
  droplevels()

# Subgroup ----------------------------------------------------------------

dt.sx <- dt. %>%
  left_join(dm. %>% select(sjid, bl.amb, itt, starts_with('subgroup'))) %>%
  filter( itt )

dt.s <- bind_rows(
  dt.sx %>% mutate ( s.group = 'all'),
  dt.sx %>% mutate ( s.group = subgroup.age    ),
  dt.sx %>% mutate ( s.group = subgroup.mFARS  ),
  dt.sx %>% mutate ( s.group = subgroup.FARS.E )
  ) %>% 
  select( -starts_with('subgroup')) %>% 
  mutate( s.group = ordered(s.group, c('all','<13.1','>13.1','<=37','>37','<22.33','>=22.33'))) %>%
  filter( !is.na(s.group))

dt.s %<>% 
  mutate(
    s.group = case_when(
      s.group == 'all'     ~ 'all',
      s.group == '<13.1'   ~ 'Age <13.1',
      s.group == '>13.1'   ~ 'Age >13.1',
      s.group == '<22.33'  ~ 'FARS.E <22.33',
      s.group == '>=22.33' ~ 'FARS.E >=22.33',
      s.group == '<=37'    ~ 'mFARS <=37',
      s.group == '>37'     ~ 'mFARS >37',      
      TRUE ~ as.character(s.group)  # Keep other levels unchanged
    )
  )

dt.s %<>% 
  mutate( group = gsub("(\\>|\\>=).*", "\\1", s.group) ) %>% 
  mutate( s.group = factor( s.group, 
    levels = c( "all", "Age <13.1", "Age >13.1", "mFARS <=37", "mFARS >37", "FARS.E <22.33", "FARS.E >=22.33")
  )) %>% 
  mutate( group = ifelse( group == 'FARS', 'FARS.E', group ) ) %>% 
  mutate( group = factor( group,
                          levels = c( "all", "Age", "mFARS", "FARS.E")
                          )) 

table(dt.s$group, dt.s$s.group, exclude = F)
  
dt.s %>% ungroup %>% 
  select(study, sjid, group, s.group) %>% 
  # filter(is.na(bl.amb)) %>%
  unique %>% group_by(study, group) %>% mutate(N = n()) %>% 
  group_by(study, group, s.group, N) %>% 
  summarise(n = n()) %>% mutate(pct = round(100*n/N,0)) %>% 
  arrange(group, study, s.group )

# not necessary: 
# dt.s %<>%
#   filter(!(study == 'FACOMS' & avisitn %in% c(0.5, 1.5)))

# zero lines --------------------------------------------------------------

bl.zero.lines <- dt.s %>%
  ungroup %>%
  filter( avisitn == 0 ) %>% 
  select( study, group, s.group, paramcd) %>% 
  unique() %>%  
  mutate(estimate = 0, avisit = 'BL' )

# MMRM --------------------------------------------------------------------

est.aval <- dt.s %>% 
  .vnames %>% 
  filter(!is.na(bl)) %>% 
  filter(avisitn <=3, avisitn > 0) %>% 
  group_by( study, group, s.group, paramcd ) %>% nest() %>% 
  mutate ( mod  = map( data, ~ lmer( cbl ~ bl + avisit + (1|sjid), data = ., control = .lme4.ctrl))) %>% 
  mutate ( emm  = map( mod , ~ emmeans::emmeans(., c('avisit')) )) %>%
  mutate ( coef = map( emm , ~ tidy ( .x, effects = 'fixed', conf.int = T ))) %>%
  select(-data, -mod, -emm) %>% 
  unnest ( coef )

est.aval %<>% 
  bind_rows( bl.zero.lines ) %>% 
  arrange( paramcd, avisit ) %>% 
  mutate ( avisit = factor(avisit, c('BL','6m','1y','18m','2y','3y','3y+')) ) %>% 
  arrange(study, s.group, avisit)

# plot --------------------------------------------------------------------

.width = 0
.dodge = .05

clr <- 'black'
clr <- NA
    
est.aval %<>% 
  left_join(dt. %>% ungroup %>% select(avisitn) %>% unique %>% .vnames) %>% 
  droplevels() 

mycols.here <- c('black','#e41a1c','#457EB7','#e41a1c','#457EB7','#e41a1c','#457EB7')

A <- est.aval %>%
  filter(s.group == 'all') %>% 
  left_join( .rt('../DATA other/scales.txt') %>% select(paramcd, param, paramlong) ) %>%
  arrange(paramcd, avisit) %>% 
  ggplot()+
  geom_errorbar(width = .width, position = position_dodge(width = .dodge), color = 'grey')+
  aes(ymin = conf.low, ymax = conf.high)+
  ggh4x::geom_pointpath( position = position_dodge(width = .dodge) )+
  aes(linetype = study)+
  aes(group = paste(study, paramcd))+
  aes(x = avisitn , y = estimate)+scale_x_continuous(breaks = unique(est.aval$avisitn), labels = levels(est.aval$avisit))+
  aes(shape = study)+.ssmA+
  aes(color = s.group)+scale_color_manual(values = mycols.here)+
  aes(group = paste(study, s.group))+
  facet_wrap(~group, scales = 'free', ncol = 2)+
  geom_hline(yintercept = 0, linetype = 2)+
  .leg_none+
  labs(x = NULL, y = NULL)

B <- est.aval %>%
  filter(group == 'Age') %>% 
  left_join( .rt('../DATA other/scales.txt') %>% select(paramcd, param, paramlong) ) %>%
  arrange(paramcd, avisit) %>% 
  ggplot()+
  geom_errorbar(width = .width, position = position_dodge(width = .dodge), color = 'grey')+
  aes(ymin = conf.low, ymax = conf.high)+
  ggh4x::geom_pointpath( position = position_dodge(width = .dodge) )+
  aes(linetype = study)+
  aes(group = paste(study, paramcd))+
  aes(x = avisitn , y = estimate)+scale_x_continuous(breaks = unique(est.aval$avisitn), labels = levels(est.aval$avisit))+
  aes(shape = study)+.ssmA+
  aes(color = s.group)+scale_color_manual(values = mycols.here[c(2,3)])+
  aes(group = paste(study, s.group))+
  facet_wrap(~group, scales = 'free', ncol = 2)+
  geom_hline(yintercept = 0, linetype = 2)+
  .leg_none+
  labs(x = NULL, y = NULL)


C <- est.aval %>%
  filter(group == 'mFARS') %>% 
  left_join( .rt('../DATA other/scales.txt') %>% select(paramcd, param, paramlong) ) %>%
  arrange(paramcd, avisit) %>% 
  ggplot()+
  geom_errorbar(width = .width, position = position_dodge(width = .dodge), color = 'grey')+
  aes(ymin = conf.low, ymax = conf.high)+
  ggh4x::geom_pointpath( position = position_dodge(width = .dodge) )+
  aes(linetype = study)+
  aes(group = paste(study, paramcd))+
  aes(x = avisitn , y = estimate)+scale_x_continuous(breaks = unique(est.aval$avisitn), labels = levels(est.aval$avisit))+
  aes(shape = study)+.ssmA+
  aes(color = s.group)+scale_color_manual(values = mycols.here[c(2,3)])+
  aes(group = paste(study, s.group))+
  facet_wrap(~group, scales = 'free', ncol = 2)+
  geom_hline(yintercept = 0, linetype = 2)+
  .leg_none+
  labs(x = NULL, y = NULL)

D <- est.aval %>%
  filter(group == 'FARS.E') %>% 
  left_join( .rt('../DATA other/scales.txt') %>% select(paramcd, param, paramlong) ) %>%
  arrange(paramcd, avisit) %>% 
  ggplot()+
  geom_errorbar(width = .width, position = position_dodge(width = .dodge), color = 'grey')+
  aes(ymin = conf.low, ymax = conf.high)+
  ggh4x::geom_pointpath( position = position_dodge(width = .dodge) )+
  aes(linetype = study)+
  aes(group = paste(study, paramcd))+
  aes(x = avisitn , y = estimate)+scale_x_continuous(breaks = unique(est.aval$avisitn), labels = levels(est.aval$avisit))+
  aes(shape = study)+.ssmA+
  aes(color = s.group)+scale_color_manual(values = mycols.here[c(2,3)])+
  aes(group = paste(study, s.group))+
  facet_wrap(~group, scales = 'free', ncol = 2)+
  geom_hline(yintercept = 0, linetype = 2)+
  .leg_none+
  labs(x = NULL, y = NULL)

ggpubr::ggarrange(A, B, C, D)

.sp( ti = paste('All and Subgroups', par. ))

