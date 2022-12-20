
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

dt.long   <- readRDS('DATA derived/dt.long.rds') %>% 
  filter(study == 'FACHILD')

dt.long %<>%
  filter(!grepl('.iu', paramcd)) %>% droplevels %>% 
  mutate( avisit = factor(avisitn)) %>% 
  left_join(
    readRDS('DATA derived/dt.bl.rds') %>% 
      rename(amb.bl = amb) %>%  
      select(study, sjid, amb.bl, med.age, med.FARS.E)
    ) %>%
  mutate(avisit = factor(avisitn)) %>% 
  select(study, sjid, avisitn, avisit, time., fpf, hpf, paramcd, param, aval, cbl, everything())

levels(dt.long$avisit) <- c('BL', '6m', '1y', '18m','2y','3y')
with(dt.long, table(avisit, avisitn))

dt.long %>% filter(is.na(aval))

# descriptive changes -----------------------------------------------------

dt.s <- dt.long %>% 
  # filter(avisitn != 6) %>% 
  group_by(study, paramcd, param, avisit, avisitn ) %>% 
  filter(!is.na(cbl)) %>% 
  summarise(n = n(), m = mean( cbl ), s = sd(cbl ), SRM = abs(m/s)) %>% 
  group_by(paramcd, param) %>% mutate(height = min(m-max(s)))

.long.plot.descr <- function (df, parids, title) {
  df %>% 
    filter(paramcd %in% pars.[parids]) %>% 
    ggplot()+geom_pointrange()+geom_line()+
    aes( avisit, m, ymin = m-s, ymax=m+s)+
    aes( group = param )+
    geom_text ( aes(label = n , y = height), size = 3 )+
    geom_hline( yintercept = 0, linetype = 'dashed')+
    facet_wrap( ~ param, scale = 'free_y')+
    scale_x_discrete(labels = c('BL', '6m', '1y', '18m', '2y','3y'))+
    .leg_ll+
    xlab('Visit')+
    ylab('Mean Change (SD)')+
    ggtitle(title)
  }

# by subgroups -----------------------------------------------------------

dt.ss <- bind_rows(
  dt.long %>% filter(avisitn != 6) %>% filter(!is.na( amb.bl    )) %>% mutate(group = amb.bl),
  dt.long %>% filter(avisitn != 6) %>% filter(!is.na( med.age   )) %>% mutate(group = med.age),
  dt.long %>% filter(avisitn != 6) %>% filter(!is.na( med.FARS.E)) %>% mutate(group = med.FARS.E)
)

dt.ss %<>% 
  filter(!is.na(group))

dt.ss %<>%
  group_by(paramcd, param, group, avisit, avisitn ) %>% 
  filter(!is.na(cbl)) %>% 
  summarise(n = n(), m = mean( cbl ), s = sd(cbl )) %>% 
  mutate(s = ifelse(is.na(s), 0, s)) %>% 
  group_by(paramcd, param, group) %>%
  mutate(height = min(m-max(s, na.rm=T), na.rm=T))

.long.plot.descr.sub <- function (df, groups, parids, title) {
  
  .dodge <- position_dodge(width = 0.1)
  
  df %>% 
    filter(group %in% groups) %>% 
    group_by(paramcd, param) %>% mutate(height = min(height)) %>% 
    filter(paramcd %in% pars.[parids]) %>% 
    ggplot()+geom_pointrange(position = .dodge)+geom_line(position = .dodge)+
    aes( avisit, m, ymin = m-s, ymax=m+s)+
    aes( shape = group )+ scale_shape_manual(values = c(21,19))+
    aes( group = group)+
    geom_text ( aes(label = n , y = height), size = 3,position = position_dodge(width = 0.6) )+
    geom_hline( yintercept = 0, linetype = 'dashed')+
    facet_wrap( ~ param, scale = 'free_y')+
    scale_x_discrete(labels = c('BL', '6m', '1y', '18m', '2y','3y'))+
    .leg_lr+
    xlab('Visit')+
    ylab('Mean Change (SD)')+
    ggtitle(title)
  }

read_pptx ( '../Templates/CR.template.pptx' ) %>%
  add_slide ( layout = 'F', master = 'CR')  %>%
  ph_with   ( dml ( print ( 
    .long.plot.descr(dt.s, c(1,2,3,4,5),    'Extended Walking, T25FW & Peg Test - Descriptive Changes'),
    newpage = F ) ), location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide ( layout = 'F', master = 'CR')  %>%
  ph_with   ( dml ( print ( 
    .long.plot.descr(dt.s, c(8,9,10,11,12), 'Rating Scales and ADL - Descriptive Changes'),
    newpage = F ) ), location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide ( layout = 'F', master = 'CR')  %>%
  ph_with   ( dml ( print (
    .long.plot.descr.sub(dt.ss, c('ambulatory','non-amb.')         ,c(1,2,3,4,5),    'Extended Walking, T25FW & Peg Test - Descriptive Changes, by Ambulation'),
    newpage = F ) ), location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide ( layout = 'F', master = 'CR')  %>%
  ph_with   ( dml ( print (
    .long.plot.descr.sub(dt.ss, c('FARS.E < 22.8','FARS.E > 22.8') ,c(1,2,3,4,5),    'Extended Walking, T25FW & Peg Test - Descriptive Changes, by Median FARS E'),
    newpage = F ) ), location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide ( layout = 'F', master = 'CR')  %>%
  ph_with   ( dml ( print (
    .long.plot.descr.sub(dt.ss, c('age < 13.9','age > 13.9')       ,c(1,2,3,4,5),    'Extended Walking, T25FW & Peg Test - Descriptive Changes, by Median Age'),
    newpage = F ) ), location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide ( layout = 'F', master = 'CR')  %>%
  ph_with   ( dml ( print (
    .long.plot.descr.sub(dt.ss, c('ambulatory','non-amb.')         ,c(8,9,10,11,12), 'Rating Scales and ADL - Descriptive Changes, by Ambulation'),
    newpage = F ) ), location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide ( layout = 'F', master = 'CR')  %>%
  ph_with   ( dml ( print (
    .long.plot.descr.sub(dt.ss, c('FARS.E < 22.8','FARS.E > 22.8') ,c(8,9,10,11,12), 'Rating Scales and ADL - Descriptive Changes, by Median FARS E'),
    newpage = F ) ), location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide ( layout = 'F', master = 'CR')  %>%
  ph_with   ( dml ( print (
    .long.plot.descr.sub(dt.ss, c('age < 13.9','age > 13.9')       ,c(8,9,10,11,12), 'Rating Scales and ADL - Descriptive Changes, by Median Age'),
    newpage = F ) ), location = ph_location_type( type = "body" , id = 1) ) %>%
  print ( target = paste('Changes.DESC.', gsub(":","-", Sys.time()), ".pptx", sep="") )

# .long.plot.descr(dt.s, c(1,2,3,4,5),    'Descriptive Changes, Extended Walking, T25FW & Peg Test'),
# .long.plot.descr(dt.s, c(8,9,10,11,12), 'Descriptive Changes, Rating Scales and ADL'),


# .long.plot.descr.sub(dt.ss, c('ambulatory','non-amb.')         ,c(1,2,3,4,5),    'Descriptive Changes by Ambulation, Extended Walking, T25FW & Peg Test'),
# .long.plot.descr.sub(dt.ss, c('FARS.E < 22.8','FARS.E > 22.8') ,c(1,2,3,4,5),    'Descriptive Changes by Median FARS E, Extended Walking, T25FW & Peg Test'),
# .long.plot.descr.sub(dt.ss, c('age < 13.9','age > 13.9')       ,c(1,2,3,4,5),    'Descriptive Changes by Median Age, Extended Walking, T25FW & Peg Test'),
# 
# .long.plot.descr.sub(dt.ss, c('ambulatory','non-amb.')         ,c(8,9,10,11,12), 'Descriptive Changes by Ambulation, Rating Scales and ADL'),
# .long.plot.descr.sub(dt.ss, c('FARS.E < 22.8','FARS.E > 22.8') ,c(8,9,10,11,12), 'Descriptive Changes by Median FARS E, Rating Scales and ADL'),
# .long.plot.descr.sub(dt.ss, c('age < 13.9','age > 13.9')       ,c(8,9,10,11,12), 'Descriptive Changes by Median Age, Rating Scales and ADL'),