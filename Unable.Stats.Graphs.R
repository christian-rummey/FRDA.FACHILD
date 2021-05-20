

# read data ---------------------------------------------------------------

rm(list=ls())

require('labelled')
source('list.pars.R')

dt.   <- readRDS('DATA derived/dt.long.rds')

dt. %>% 
  ungroup %>% 
  select(study, sjid, avisitn) %>% 
  unique

dt. %<>%
  filter( paramcd %in% pars.[c(1,2,3,  6,7)] ) %>% 
  droplevels

table(dt.$paramcd)
table(dt.$param)

# need to fill these unables
dt. %<>% 
  mutate(unable = ifelse(paramcd == 'w6m' & is.na(aval) & is.na(unable), T, unable )) %>%   
  mutate(unable = ifelse(paramcd == 'w1m' & is.na(aval) & is.na(unable), T, unable ))

# unable line graph -------------------------------------------------------

# dt.$age %>% unique %>% cut_number(10) # c(6.6, 10.7, 12.1, 13.1, 13.9, 14.7, 15.7, 16.4, 17.2, 18, 20.3)
# dt.$age %>% unique %>% cut_number(8) # c(6.6, 11, 12.6, 13.7, 14.7, 15.9, 16.9, 17.8, 20.3)
# dt.$age %>% unique %>% cut_number(6) # c(6.6, 11.7, 13.4, 14.7, 16.3, 17.5, 20.3)

p1 <- dt. %>% #group_by(unable) 
  ungroup %>% 
  select(study, sjid, age, param, unable) %>% 
  mutate(age = cut(age, c(6.6, 11.7, 13.4, 14.7, 16.3, 17.5, 20.3))) %>% 
  group_by(param, age) %>% 
  mutate(n = n()) %>% #select(n) %>% table
  group_by(param, age, unable, n) %>%  
  summarise(ptc = 100*n()/n) %>% 
  filter(unable == T) %>% 
  mutate_at('param', factor, levels(dt.$param)[c(3,1,2,4,5)]) %>% 
  ggplot()+geom_line()+geom_point()+
  aes(x = age, y = ptc)+
  aes(color = param)+
  aes(group = param)+
  # geom_col()+
  # facet_wrap( ~ paramcd  )+
  geom_hline(yintercept = c(0,50,100), linetype = 'dotted')+
  theme_minimal(base_size = 16)+
  ggtitle('Percentage Unable by Age')+theme(plot.title = element_text(hjust = 0.5))+
  xlab('Age (equally sized bins)')+
  ylab('Percentage Unable')+
  .leg_tl

p1

# unable line graph - by FARS.E ---------------------------------------------

tmp <- readRDS('DATA derived/dt.long.rds') %>% 
  ungroup %>% filter(paramcd == 'mFARS') %>% select(study, sjid, avisitn, aval) %>% rename(mFARS = aval) %>% 
  mutate(cut = cut_number(mFARS,6))

p2 <- dt. %>% #group_by(unable) 
  left_join(tmp) %>% 
  filter(!is.na(cut)) %>% 
  ungroup %>% 
  select(study, sjid, cut, param, unable) %>% 
  group_by(param, cut) %>% 
  mutate(n = n()) %>% #select(n) %>% table
  group_by(param, cut, unable, n) %>%  
  summarise(ptc = 100*n()/n) %>% 
  filter(unable == T) %>% 
  mutate_at('param', factor, levels(dt.$param)[c(3,1,2,4,5)]) %>% 
  ggplot()+geom_line()+geom_point()+
  aes(x = cut, y = ptc)+
  aes(color = param)+
  aes(group = param)+
  geom_hline(yintercept = c(0,50,100), linetype = 'dotted')+
  theme_minimal(base_size = 16)+
  ggtitle('Percentage Unable by mFARS')+theme(plot.title = element_text(hjust = 0.5))+
  xlab('mFARS (equally sized bins)')+
  ylab('Percentage Unable')+
  .leg_tl

p2

# unable line graph - by FARS.E ---------------------------------------------

tmp <- readRDS('DATA derived/dt.long.rds') %>% 
  ungroup %>% filter(paramcd == 'FARS.E') %>% select(study, sjid, avisitn, aval) %>% rename(FARS.E = aval) %>% 
  mutate(cut = cut_number(FARS.E,6))

p3 <- dt. %>% #group_by(unable) 
  left_join(tmp) %>% 
  filter(!is.na(cut)) %>% 
  ungroup %>% 
  select(study, sjid, cut, param, unable) %>% 
  group_by(param, cut) %>% 
  mutate(n = n()) %>% #select(n) %>% table
  group_by(param, cut, unable, n) %>%  
  summarise(ptc = 100*n()/n) %>% 
  filter(unable == T) %>% 
  mutate_at('param', factor, levels(dt.$param)[c(3,1,2,4,5)]) %>% 
  ggplot()+geom_line()+geom_point()+
  aes(x = cut, y = ptc)+
  aes(color = param)+
  aes(group = param)+
  geom_hline(yintercept = c(0,50,100), linetype = 'dotted')+
  theme_minimal(base_size = 16)+
  ggtitle('Percentage Unable by FARS E')+theme(plot.title = element_text(hjust = 0.5))+
  xlab('FARS E (equally sized bins)')+
  ylab('Percentage Unable')+
  .leg_tl

p3

# unable barchart ---------------------------------------------------------

p4 <- dt. %>%   
  mutate(age = cut(age, c(6.6, 10, 12, 13, 14, 15.5, 16.5, 17.5, 18.5, 20.3))) %>%
  ungroup %>% 
  ggplot()+geom_bar()+
  aes( x = age )+
  aes(fill = unable)+
  scale_fill_brewer(palette = 'Set1', type = 'q', direction = -1)+
  facet_wrap(~param)+
  theme_minimal(base_size = 12)+
  ggtitle('Unable/No Data by Age')+theme(plot.title = element_text(hjust = 0.5))+
  xlab('Age')+
  ylab('Number of Observations')+
  .leg_lr

p4

# ? -----------------------------------------------------------------------

dt.cat <- .dd.FA('scafi_cat', c = T) %>% filter(study == 'FACHILD')

dt. %>%
  rename(unable.TF = unable) %>%
  left_join(dt.cat)
# 
# with(dt., table(unable.TF, unable), exclude = F)
# with(dt., table(unable.TF, device), exclude = F)

# filter(dt., unable.TF, device == 1) %>% 
#   select(avisitn, paramcd, unable.TF, device, aval)
# 
# w16unable <- dt. %>% 
#   filter(paramcd %in% c('w1m','w6m')) %>% 
#   select(sjid, avisitn, paramcd, unable.TF) %>% 
#   spread(paramcd, unable.TF) %>% 
#   filter(w1m==F, w6m==T) %>% 
#   mutate(w16unable = T) %>% 
#   select(sjid, avisitn, w16unable)

x <- dt. %>% 
  # left_join(w16unable) %>% filter(w16unable) %>% 
  filter(paramcd != 'hpt.i') %>% 
  # group_by(sjid, avisitn) %>% 
  # filter   ( unable.TF == F ) %>%
  mutate_at('unable', ~ ifelse(is.na(.)  , 0 , unable) ) %>%
  mutate_at('unable', ~ factor(unable, c(0,3.1, 4.1, 1, 2, 3, 4)))

levels(x$unable) <- c('-','one trial (second fatigue)','one trial (second refused)','unable, progr.', 'unable, other', 'fatigue','refused' )
# 
# x %>% 
#   # mutate_at('device', ~ ifelse(unable.TF , NA, device) ) %>% 
#   # mutate_at('device', ~ ifelse(is.na(.)  , -1, device) ) %>% 
#   # mutate(age = cut(age, c(6.6, 9, 12, 15, 19.1), labels = c('\u22649','10-12','13-15', '16-19'))) %>% 
#   mutate(age = cut(age, seq(6, 21, 3), labels = c('\u22649','10-12','13-15','16-18','>18'))) %>% 
#   mutate(FARS.E = cut(FARS.E, seq(0, 36, 6))) %>%
#   # filter(!is.na(FARS.E)) %>% 
#   ggplot()+geom_bar()+
#   aes( x = FARS.E )+
#   aes(fill = factor(unable))+
#   # scale_fill_manual(values = c('grey', RColorBrewer::brewer.pal(3, 'Set1'))[c(1,3,4)], labels = c('unable', '-', 'device' ))+
#   scale_fill_manual(values = c('#3182bd','#9ecae1','#c6dbef','grey','darkgrey','#fec44f','#d95f0e'))+
#   facet_wrap(~param, ncol = 3)+
#   # geom_hline(yintercept = c(0,50,100), linetype = 'dotted')+
#   theme_minimal(base_size = 12)+
#   ggtitle('Unable/No Data by Age')+theme(plot.title = element_text(hjust = 0.5))+
#   xlab('FARS.E')+
#   ylab('Number of Observations')+
#   .leg_lrh

# .sp('Device use by Age, unable w6m in Walking Tests')

# by t25fw ----------------------------------------------------------------

# dt. %>% 
#   filter(paramcd == 'w25.i') %>% 
#   select(sjid, avisitn, aval, unable.TF)
# filter(w25.i, !w6m)




# 
#   
#   rename( w25.i = adt25 , tug.i = adtug, )
#   gather( name, val, contains('t25') , contains('tug'), contains('ex')) %>% 
#   mutate( paramcd = NA ) %>% 
#   mutate( paramcd = ifelse( grepl ( 't25'  , name), 'w25', paramcd)) %>% 
#   mutate( paramcd = ifelse( grepl ( 'tug'  , name), 'tug', paramcd)) %>% 
#   mutate( paramcd = ifelse( grepl ( 'ex', name), 'ex', paramcd)) %>% 
#   mutate( paramcd = ifelse( grepl ( 'ex', name), 'ex', paramcd)) %>% 
#   mutate( type = NA ) %>% 
#   mutate( type = ifelse( grepl ( 'ad', name), 'device', type)) %>% 
#   mutate( type = ifelse( grepl ( 'cp', name), 'unable', type)) %>% 
#   # select( -name) %>%
#   spread( paramcd, type)
#   
# 
# 
# dt. %>% 
#   group_by(sjid, avisitn) %>% 
#   filter(sjid %in% c('4866', '4926', '5006', '4970')) %>% 
#   arrange(sjid, avisitn) %>% 
#   select(starts_with('ad'))
# 
# dt. %<>% 
#   mutate( dhrchy.r = paste( adt251, adtug, adext, sep = '.'  )) %>%
#   mutate( hrchy.r  = paste( cptt25, cptug, cpexm1, cpexm6, sep = '.' )) %>%
#   # mutate_at( names(dt.)[c(4:10)], ~ ifelse(  is.na(.), 0, .) ) %>% 
#   mutate_at( names(dt.)[c(4:10)], round, 0 ) %>%
#   mutate_at( names(dt.)[c(4:10)], ~ ifelse(.>0, 1, 0) ) %>% 
#   mutate( dhrchy   = paste( adt251, adtug, adext , sep = '.')) %>%
#   mutate( hrchy    = paste( cptt25, cptug, cpexm1, cpexm6 , sep = '.')) %>%
#   mutate( cat = NA) %>% 
#   mutate( cat = ifelse(hrchy %in% c('NA.NA.NA.NA'), 'done all 4', NA)) %>% 
#   mutate( cat = ifelse(hrchy %in% c('0001'), 'done 3', cat)) %>% 
#   mutate( cat = ifelse(hrchy %in% c('0011'), 'done 2', cat)) %>% 
#   mutate( cat = ifelse(hrchy %in% c('0111'), 'done 1', cat)) %>% 
#   mutate( cat = ifelse(hrchy %in% c('1111'), 'all unable, progr.', cat)) %>% 
#   # mutate( cat = factor( cat, c('done all 4', 'done 3', 'done 2', 'done 1','all unable, progr.'))) %>% 
#   mutate()
#   
# table(dt.$cat, dt.$hrchy)
# 
# dt. %>% 
#   ggplot()+geom_bar()+
#   aes(fill = dhrchy)+
#   aes(x = cat)
