
# read data ---------------------------------------------------------------

rm(list=ls())

require('labelled')
source('list.pars.R')

dt.   <- readRDS('DATA derived/dt.long.rds')

# follow up type barchart ----------------------------

dt.$avisitn <- factor(dt.$avisitn)

levels(dt.$avisitn) <- c('BL','6m','1y','1.5y','2y', '3y')

dt <- dt. %>%
  ungroup %>% select  ( study, sjid, avisitn ) %>%
  group_by( study, sjid, avisitn ) %>%
  unique() %>% #filter(n()>1 )
  ungroup %>% 
  mutate(N   = length(unique(sjid))) %>% 
  group_by( study, sjid, avisitn ) %>%
  group_by(avisitn, N) %>% 
  count() %>% 
  mutate(pct = round(100*n/(N), 0))

dt %>% 
  ggplot()+geom_col()+
  # aes(x = avisitn, y = pct) +
  aes(x = c(0, 0.5,1, 1.5, 2, 3), y = pct) +
  scale_x_continuous(breaks = c(0,0.5,1, 1.5, 2, 3), labels = c('BL','6m','1y','1.5y','2y','3y'))+
  geom_text(aes(label = paste(n,'\n(',pct,'%)', sep = '')), nudge_y = -5, color = 'white')+
  # coord_flip(ylim = c(0,112))+
  # ggrepel::geom_text_repel(aes(label = paste0( round( pct,0 ), '%')), direction = 'x', nudge_y = 5)+
  # ggtitle('Visits per Patient')+theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal(base_size = 16)+
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank())+
  xlab('Follow Up Visit')+
  ylab('Subjects (%)')

dt <- dt. %>%
  arrange(hpf)

list.ipv <- dt %>%
  # filter(sjid == 4711) %>% filter(paramcd == 'w6m')
  filter(!grepl('.iu', paramcd)) %>% 
  ungroup %>% select  ( study, sjid, adt, avisitn, hpf ) %>%
  group_by( study, sjid, adt, avisitn, hpf ) %>%
  unique()  %>% 
  group_by( study, sjid, adt, avisitn ) %>%
  filter(n()>1) %>% 
  left_join(.dd.FA('demo') %>% select(sjid, site))%>% 
  arrange(sjid, adt, avisitn) 


# errors - for reporting later --------------------------------------------

dt %>% 
  filter(!is.na(aval)) %>% 
  filter(!grepl('.iu', paramcd)) %>% 
  filter(sjid %in% list.ipv$sjid) %>% 
  ungroup() %>% 
  select(study, sjid, adt, avisitn, hpf, paramcd) %>%
  right_join(list.ipv) %>% 
  spread(paramcd, hpf) %>% 
  arrange(adt) %>% 
  filter(sjid == 5136)

.rd.FACHILD('exttw')   %>% filter(PATNO == 'UF9')  %>% select(PATNO, EVENT_ID, C_INFODT, FORMPERF, HOWPERF)
.rd.FACHILD('t25fw')   %>% filter(PATNO == 'UF9')  %>% select(PATNO, EVENT_ID, C_INFODT, FORMPERF, HOWPERF)
.rd.FACHILD('faneuro') %>% filter(PATNO == 'UF9')  %>% select(PATNO, EVENT_ID, C_INFODT, FORMPERF, HOWPERF)
.rd.FACHILD('exttw')   %>% filter(PATNO == '4770') %>% select(PATNO, EVENT_ID, C_INFODT, FORMPERF, HOWPERF)
.rd.FACHILD('faadl')   %>% filter(PATNO == '4770') %>% select(PATNO, EVENT_ID, C_INFODT, FORMPERF, HOWPERF)
.rd.FACHILD('exttw')   %>% filter(PATNO == 'UF12') %>% select(PATNO, EVENT_ID, C_INFODT, FORMPERF, HOWPERF)
.rd.FACHILD('bbs')     %>% filter(PATNO == 'UF12') %>% select(PATNO, EVENT_ID, C_INFODT, FORMPERF, HOWPERF)

dt %>% 
  # filter(sjid == 5034) %>% filter(avisitn == '2y') %>% 
  filter(sjid == 5136) %>% filter(avisitn == '1.5y')
ungroup %>% select  ( study, sjid, adt, avisitn, hpf, paramcd, aval ) %>%
  group_by( study, sjid, avisitn, hpf ) %>%
  unique()  %>% 
  group_by( study, sjid, avisitn ) %>%
  filter(sjid == 5034, avisitn == '2y')
  

%>% arrange( sjid, avisitn )
  group_by()
  ungroup %>% 
  mutate(N   = length(unique(sjid))) %>% 
  group_by( study, sjid, avisitn, hpf ) %>%
  group_by(avisitn, hpf, N) %>% 
  count() %>% 
  mutate(pct = round(100*n/(N), 0)) %>% 
  ggplot()+ geom_col()+
  aes(x = avisitn, y = pct, fill = hpf) + 
  scale_fill_manual(values = c('lightblue','darkgrey'))+
  # geom_text(aes(label = paste(n), nudge_y = -5, color = 'white'))+
  # geom_text(aes(label = pct), nudge_y = -5)+
  # coord_flip(ylim = c(0,112))+ggrepel::geom_text_repel(aes(label = paste0( round( pct,0 ), '%')), direction = 'x', nudge_y = 5)+
  # facet_wrap(~paramcd)+
  theme_minimal(base_size = 16)+
  # theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank())+
  # ggtitle('Visits per Patient')+theme(plot.title = element_text(hjust = 0.5))+
  xlab('Follow Up Visit')+
  ylab('Subjects (%)')+
  .leg_tr

tmp <- dt. %>%
  # mutate(hpf = ifelse(is.na(hpf) | hpf == 'In-Person', 'In-Person', 'Video/Audio')) %>% 
  # mutate(hpf = factor(hpf, c('Video/Audio', 'In-Person'))) %>% 
  # mutate(hpf = ifelse(is.na(hpf) | hpf == 'In-Person', 'In-Person', 'Video/Audio')) %>% 
  # mutate(hpf = factor(hpf, c('In-Person', 'Video/Audio' ))) %>% 
  mutate(hpf = ifelse(is.na(hpf), 'In-Person', hpf)) %>% 
  mutate(hpf = factor(hpf, c('In-Person', 'Video', 'Audio Only' ))) %>%
  arrange(hpf)

stat <- .ds.FACHILD('concl') %>% mutate_at('sjid', as.character) %>% select(sjid, cnclstat)

tmp %>% 
  left_join(stat) %>% 
  filter(!is.na(cnclstat)) %>% 
  group_by(sjid) %>% 
  select(sjid, avisitn, adt) %>% 
  unique %>% 
  filter(adt == max(adt)) %>% 
  ungroup %>% 
  select(avisitn) %>% table

tmp %>% 
  ungroup %>% select  ( study, sjid, avisitn, hpf ) %>%
  group_by( study, sjid, avisitn, hpf ) %>%
  unique() %>% #filter(n()>1 )
  ungroup %>% 
  mutate(N   = length(unique(sjid))) %>% 
  group_by( study, sjid, avisitn, hpf ) %>%
  group_by(avisitn, hpf, N) %>% 
  count() %>% 
  mutate(pct = round(100*n/(N), 0)) %>% 
  ungroup %>% 
  select(-N, -pct) %>% 
  spread(hpf, n)






read_pptx( '../Templates/CR.template.pptx' ) %>%
  add_slide   ( layout = 'TTE', master = 'CR') %>%
  ph_with     ( p1, location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide   ( layout = 'TTE', master = 'CR') %>%
  ph_with     ( p2, location = ph_location_type( type = "body" , id = 1) ) %>%
  print ( target = paste('Visits.Overview.', gsub(":","-", Sys.time()), ".pptx", sep="") )


# .sp('Follow Up Visits')


# # older versions ----------------------------------------------------------
# 
# dt. <- bind_rows(
#   .dd.FA('fars'),
#   .dd.FA('scafi'),
#   .dd.FA('bbs')
#   ) %>% 
#   filter( study == 'FACHILD' ) %>% 
#   select( study, sjid, avisitn, adt ) %>%
#   unique() %>% 
#   group_by(sjid, avisitn, adt) %>%
#   .add.demo()
# 
# dt. %>%
#   group_by(sjid) %>% 
#   count(sjid) %>% 
#   group_by(n) %>% 
#   count() %>% 
#   ggplot()+geom_col()+
#   aes(x = n, y = nn, label = nn) +
#   coord_flip()+ggrepel::geom_text_repel(direction = 'x', nudge_y = 1)+
#   xlab('Number of Visits')+
#   theme_minimal(base_size = 16)+
#   ggtitle('Visits per Patient')+theme(plot.title = element_text(hjust = 0.5))+
#   ylab('')
#   
# # .spi(ti = 'Visit Counts', l = '2', i = 1)
# # getwd()
# # .ti()
# 
# 
# 
# # follow up type barchart by age group ----------------------------
# 
# dt. %>%
#   mutate( paramcd = factor(paramcd, pars.[c(1,2,3,4,8)]) ) %>% 
#   filter(!is.na(paramcd)) %>% 
#   select( study, sjid, avisitn, age.groups, paramcd ) %>%
#   unique() %>% 
#   group_by( avisitn, age.groups, paramcd) %>%
#   mutate(n = length(unique(sjid))) %>% 
#   count() %>% 
#   ggplot()+geom_col()+
#   aes(x = avisitn, y = n, label = n) +
#   coord_flip()+ggrepel::geom_text_repel(direction = 'x', nudge_y = 1)+
#   facet_grid(age.groups~paramcd)+
#   xlab('Number of Visits')+
#   theme_minimal(base_size = 16)+
#   ggtitle('Visits per Patient')+theme(plot.title = element_text(hjust = 0.5))+
#   ylab('')
# 
# # .sp('Visit Counts')
# # getwd()
# # .ti()
# 
# 
# # 14 patients without follow-up?
# .dd.FA('fars') %>% 
#   filter(study == 'FACHILD') %>% 
#   filter(paramcd == 'mFARS') %>% 
#   group_by(sjid) %>% 
#   filter(n()==1)
# 
# 
# 
