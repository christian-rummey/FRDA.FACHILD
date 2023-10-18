
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

parids <- c(1,2,3,4)  # "mFARS"  "FARS.E" "FARS.B" "FARS.C"
# parids <- c(6,8,9,10) # "w25.i"  "tug.i"   "w1m"     "w6m"
# parids <- c(13,14,15,16) # PedsQL Total and Physical

pars..   <- pars.[c(parids)] 

# Subgroup ----------------------------------------------------------------

dt.s <- dt. %>% 
  filter(paramcd %in% pars..) %>% 
  left_join(dm. %>% select(sjid, bl.amb, itt, starts_with('subgroup'), pm)) %>%
  filter( itt ) %>%
  mutate  ( s.group = ifelse(pm %in% c('G130V','I154F'), 'G130V/I154F' , 'all others' ) ) %>% 
  # filter( study == 'FACHILD') %>%
  # filter( bl.amb == 'ambulatory' ) %>%
  # mutate  ( s.group = 1) %>%
  # mutate( s.group = subgroup.age ) %>%
  # mutate( s.group = subgroup.mFARS ) %>%
  # mutate( s.group = subgroup.FARS.E ) %>%
  # mutate( s.group = s.group.age ) %>%
  # mutate( s.group = bl.amb ) %>% #mutate(s.group = ifelse(is.na(s.group), 'non-amb.', s.group)) %>%
  filter( !is.na( s.group )) %>% 
  droplevels()

dt.s %>% ungroup %>% 
  select(study, sjid, s.group) %>% 
  # filter(is.na(bl.amb)) %>%
  unique %>% group_by(study) %>% mutate(N = n()) %>% 
  group_by(study, s.group, N) %>% 
  summarise(n = n()) %>% mutate(pct = round(100*n/N,0))

# title slide -------------------------------------------------------------

dt.s %<>%
  filter(!(study == 'FACOMS' & avisitn %in% c(0.5, 1.5)))

# zero lines --------------------------------------------------------------

bl.zero.lines <- dt.s %>%
  ungroup %>%
  filter( avisitn == 0 ) %>% 
  select(study, s.group, paramcd) %>% 
  unique() %>%  
  mutate(estimate = 0, avisit = 'BL' )

# MMRM --------------------------------------------------------------------

est.aval <- dt.s %>% 
  filter(paramcd %in% pars..) %>% 
  # .vnames %>% 
  filter(!is.na(bl)) %>% 
  filter(avisitn <=3, avisitn > 0) %>% 
  group_by( study, s.group, paramcd ) %>% nest() %>% 
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
.dodge = .075

clr <- 'black'
clr.errbars <- 'darkgrey'
clr <- NA
    
est.aval %<>% 
  left_join(dt. %>% ungroup %>% select(avisitn) %>% unique %>% .vnames) %>% 
  droplevels() 

if (est.aval$s.group[1] != 1) {
  
A <- est.aval %>%
  filter(paramcd %in% c('mFARS','FARS.E')) %>%
  arrange(paramcd, avisit) %>% 
  ggplot()+
  geom_errorbar(width = .width, position = position_dodge(width = .dodge), color = clr.errbars)+
  aes(ymin = conf.low, ymax = conf.high)+
  ggh4x::geom_pointpath( position = position_dodge(width = .dodge) )+
  aes(linetype = study)+
  aes(group = paste(study, paramcd))+
  aes(x = avisitn , y = estimate)+scale_x_continuous(breaks = unique(est.aval$avisitn), labels = levels(est.aval$avisit))+
  aes(shape = study)+.ssmA+
  aes(color = s.group)+scale_color_manual(values = c('#e41a1c','#457EB7','#e41a1c','#457EB7'))+
  aes(group = paste(study, s.group))+
  facet_wrap(~paramcd, scales = 'free', ncol = 2)+
  geom_hline(yintercept = 0, linetype = 2)+
  .leg_none+
  xlab('Visit')+
  ylab('XXX')

B <- est.aval %>%
  filter(!(paramcd %in% c('mFARS','FARS.E'))) %>%
  arrange(paramcd, avisit) %>% 
  ggplot()+
  geom_errorbar(width = .width, position = position_dodge(width = .dodge), color = clr.errbars)+
  aes(ymin = conf.low, ymax = conf.high)+
  ggh4x::geom_pointpath( position = position_dodge(width = .dodge) )+
  aes(linetype = study)+
  aes(group = paste(study, paramcd))+
  aes(x = avisitn , y = estimate)+scale_x_continuous(breaks = unique(est.aval$avisitn), labels = levels(est.aval$avisit))+
  aes(shape = study)+.ssmA+
  aes(color = s.group)+scale_color_manual(values = c('#e41a1c','#457EB7','#e41a1c','#457EB7'))+
  aes(group = paste(study, s.group))+
  facet_wrap(~paramcd, scales = 'free', ncol = 2)+
  geom_hline(yintercept = 0, linetype = 2)+
  .leg_none+
  xlab('Visit')+
  ylab('Estimated Change from Baseline (95%CI)')

} else {
    
A <- est.aval %>% 
  filter(paramcd %in% c('mFARS','FARS.E')) %>%
  arrange(paramcd, avisit) %>% 
  ggplot()+
  geom_errorbar(width = .width, position = position_dodge(width = .dodge), color = clr.errbars)+
  aes(ymin = conf.low, ymax = conf.high)+
  ggh4x::geom_pointpath( position = position_dodge(width = .dodge) )+
  aes(linetype = study)+
  aes(group = paste(study, paramcd))+
  aes(x = avisitn , y = estimate)+scale_x_continuous(breaks = unique(est.aval$avisitn), labels = levels(est.aval$avisit))+
  aes(shape = study)+.ssmA+
  aes(group = paste(study))+
  facet_wrap(~paramcd, scales = 'free', ncol = 2)+
  .leg_none+
  xlab('Visit')+
  ylab('XXX')

B <- est.aval %>% 
  filter(!(paramcd %in% c('mFARS','FARS.E'))) %>%
  arrange(paramcd, avisit) %>% 
  ggplot()+
  geom_errorbar(width = .width, position = position_dodge(width = .dodge), color = clr.errbars)+
  aes(ymin = conf.low, ymax = conf.high)+
  ggh4x::geom_pointpath( position = position_dodge(width = .dodge) )+
  aes(linetype = study)+
  aes(group = paste(study, paramcd))+
  aes(x = avisitn , y = estimate)+scale_x_continuous(breaks = unique(est.aval$avisitn), labels = levels(est.aval$avisit))+
  aes(shape = study)+.ssmA+
  aes(group = paste(study))+
  facet_wrap(~paramcd, scales = 'free', ncol = 2)+
  .leg_none+
  xlab('Visit')+
  ylab('Estimated Change from Baseline (95%CI)')

}

# ggpubr::ggarrange(A + coord_cartesian( ylim = c(-1  , 8)),
#                   B + coord_cartesian( ylim = c(-1.5, 4)),
#                   nrow = 2
# )
ggpubr::ggarrange(A + scale_y_continuous(breaks = c(0,4,8)) + coord_cartesian( ylim = c(-1  , 10)),
                  B + scale_y_continuous(breaks = c(0,2,4)) + coord_cartesian( ylim = c(-1.5, 4)),
                  nrow = 2
)

# .sp( ti = 'title' )

