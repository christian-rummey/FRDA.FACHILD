
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
  filter( study == 'FACHILD') %>%
  filter( paramcd %in% c('mFARS','FARS.E','FARS.B','FARS.C') ) %>%
  droplevels

# Subgroup ----------------------------------------------------------------

dt.s <- dt. %>% 
  # mutate( s.group = s.group.FE ) %>% 
  # mutate( s.group = s.group.mF ) %>% 
  # mutate( s.group = s.group.age ) %>%
  # mutate( s.group = s.group.amb ) %>%
  mutate  ( s.group = 1) %>% 
  filter( !is.na( s.group ))

# title slide -------------------------------------------------------------

title <- 'F, mF'

# dev < 3M adjusted, plus add > 2y ----------------------------------------
# legacy 

dt. %<>%
  group_by( study, sjid, paramcd, avisitn ) %>%
  # mutate  ( flag.adj = ifelse ( window.dev == min(window.dev), T, F ) ) %>% 
  # filter  ( !flag.adj ) %>% ungroup %>% select(paramcd) %>% table
  arrange( study, avisitn, paramcd ) %>%
  # .vnames %>%
  # group_by( sjid ) %>% filter(max (window.dev)>0.25) %>% 
  # filter  ( time. >= 2.25 & time. < 3.5 ) %>%
  # mutate  ( dev.cat = as.character(dev.cat)) %>% 
  # mutate  ( dev.cat = ifelse(dev.cat == '>3M' & time. >  2.25 & time. < 2.5 , '>3M.ok', dev.cat )) %>%
  # mutate  ( dev.cat = ifelse(dev.cat == '>3M' & time. >= 2.5  & time. < 3.5 , '>3M.ok', dev.cat )) %>%
  # select ( -vname ) %>% .vnames %>%
  # mutate  ( dev.cat = factor(dev.cat, c(levels(dt.adj$dev.cat), '>3M.ok'))) %>% 
  # filter (avisitn < 4) %>%
  droplevels()

# window adjustment -------------------------------------------------------

dt. %<>%
  mutate( avisitn = ifelse(time. < 2.25, round(time.*2)/2, round(time.) ) ) %>%
  # mutate( flag = ifelse(avisitn.x != avisitn, T, F)) %>%
  # mutate( avisitn = avisitn.x ) %>%
  # mutate( window.dev = abs( time. - avisitn ) ) %>%
  # mutate( dev.cat    = cut(window.dev, c(0, 0.25, 0.5, 1, 10), labels = c('<3M', '>3M', '>6M', '>1y'), include.lowest = T ) ) %>% 
  droplevels

# filter duplicate visits (after adjustement)

print('all mFARS visits: ')
dt. %>% filter(paramcd == 'mFARS', study == 'FACHILD') %>% nrow()
print('all mFARS FU (BL + FU: ')
dt. %>% filter(paramcd == 'mFARS', study == 'FACHILD') %>% filter(!is.na(bl)) %>% group_by(sjid) %>% filter(n()>1) %>% nrow()

# remove FACOMS after adjustment ------------------------------------------

dt. %<>% 
  filter(!(study == 'FACOMS' & avisitn %in% c(0.5, 1.5)))

# dt. %<>% 
#   filter(bl.amb == 'ambulatory')


# subgroup summary --------------------------------------------------------
# 
# dt. %>% 
#   select()
# 
# dt. %>% 
#   filter(paramcd == 'mFARS') %>% 
#   ungroup %>% select(study, avisitn, s.group) %>% group_by(avisitn, study, s.group) %>% summarise(n = n()) %>% 
#   spread(study, n) %>% .ct

# zero lines --------------------------------------------------------------

bl.zero.lines <- dt.s %>%
  ungroup %>%
  filter( avisitn == 0 ) %>% 
  select(study, s.group, paramcd) %>% 
  unique() %>%  
  mutate(estimate = 0, avisit = 'BL' )

# MMRM --------------------------------------------------------------------

est.aval <- dt.s %>% 
  .vnames %>% 
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

clr <- 'black'
clr <- NA
    
est.aval %<>% 
  left_join(dt. %>% ungroup %>% select(avisitn) %>% unique %>% .vnames) %>% 
  droplevels() 

if (est.aval$s.group[1] != 1) {

  est.aval %>% 
    arrange(paramcd, avisit) %>% 
    ggplot()+
    # geom_errorbar(width = .1, position = position_dodge(width = .05), color = 'grey')+
    # aes(ymin = conf.low, ymax = conf.high)+
    ggh4x::geom_pointpath( position = position_dodge(width = .05) )+
    aes(linetype = study)+
    aes(group = paste(study, paramcd))+
    aes(x = avisitn , y = estimate)+scale_x_continuous(breaks = unique(est.aval$avisitn), labels = levels(est.aval$avisit))+
    aes(shape = study)+.ssmA+
    aes(color = s.group)+scale_color_manual(values = c('#e41a1c','#457EB7','#e41a1c','#457EB7'))+
    aes(group = paste(study, s.group))+
    facet_wrap(~paramcd, scales = 'free', ncol = 2)+
    geom_hline(color = clr, aes(yintercept = 10 ), data = filter(est.aval, paramcd %in% c('mFARS','FARS.E')))+
    geom_hline(color = clr, aes(yintercept = -0.5 ), data = filter(est.aval, paramcd %in% c('mFARS','FARS.E')))+
    geom_hline(color = clr, aes(yintercept = 4  ), data = filter(est.aval, !(paramcd %in% c('mFARS','FARS.E'))))+
    geom_hline(color = clr, aes(yintercept = -1.5 ), data = filter(est.aval, !(paramcd %in% c('mFARS','FARS.E'))))+
    .leg_tl+
    xlab('Visit')+
    ylab('Change from Baseline (MMRM)')
  
} else {
    
  est.aval %>% 
    arrange(paramcd, avisit) %>% 
    ggplot()+
    # geom_errorbar(width = .1, position = position_dodge(width = .05), color = 'grey')+
    # aes(ymin = conf.low, ymax = conf.high)+
    ggh4x::geom_pointpath( position = position_dodge(width = .05) )+
    aes(linetype = study)+
    aes(group = paste(study, paramcd))+
    aes(x = avisitn , y = estimate)+scale_x_continuous(breaks = unique(est.aval$avisitn), labels = levels(est.aval$avisit))+
    aes(shape = study)+.ssmA+
    # aes(color = s.group)+scale_color_manual(values = c('#e41a1c','#457EB7','#e41a1c','#457EB7'))+
    aes(group = paste(study))+
    facet_wrap(~paramcd, scales = 'free', ncol = 2)+
    geom_hline(color = clr, aes(yintercept = 10 ), data = filter(est.aval, paramcd %in% c('mFARS','FARS.E')))+
    geom_hline(color = clr, aes(yintercept = -0.5 ), data = filter(est.aval, paramcd %in% c('mFARS','FARS.E')))+
    geom_hline(color = clr, aes(yintercept = 4  ), data = filter(est.aval, !(paramcd %in% c('mFARS','FARS.E'))))+
    geom_hline(color = clr, aes(yintercept = -1.5 ), data = filter(est.aval, !(paramcd %in% c('mFARS','FARS.E'))))+
    .leg_tl+
    xlab('Visit')+
    ylab('Change from Baseline (MMRM)')

  }

.sp( ti = title )
 