
# read data ---------------------------------------------------------------

rm(list=ls())

require('labelled')
source('list.pars.R')

dt.   <- readRDS('DATA derived/dt.long.rds')

dt. %>% 
  ungroup %>% 
  select(study, sjid, avisitn) %>% 
  unique

.rd.FACHILD('exttw') %>% filter(HOWPERF != 1)
.rd.FACHILD('bbs')   %>% filter(HOWPERF != 1)
.rd.FACHILD('t25fw') %>% filter(HOWPERF != 1)
.rd.FACHILD('tug')   %>% filter(HOWPERF != 1)


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

p1 <- dt %>% 
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

p1

# video visits -------------------------------------------------------

dt. %>%
  ungroup %>% 
  # mutate(hpf = ifelse( is.na(hpf), 'filler', as.character(hpf))) %>% 
  select( -fpf ) %>% 
  filter  ( !grepl('.iu', paramcd) ) %>% 
  select  ( study, sjid, avisitn, param, hpf) %>% 
  group_by( study, sjid, avisitn, param) %>%
  filter  ( hpf %in% c('Audio Only','Video')) %>% 
  ungroup %>% 
  select  ( -param, -hpf, -param) %>% 
  unique %>% 
  arrange(sjid, avisitn) %>% 
  group_by(avisitn) %>% 
  count


p2 <- dt. %>%
  ungroup %>% 
  # mutate(hpf = ifelse( is.na(hpf), 'filler', as.character(hpf))) %>% 
  select( -fpf ) %>% 
  filter  ( !grepl('.iu', paramcd) ) %>% 
  select  ( study, sjid, avisitn, param, hpf) %>% 
  group_by( study, sjid, avisitn, param) %>%
  filter  ( hpf %in% c('Audio Only','Video')) %>% 
  ggplot()+geom_bar()+
  aes(x = param)+
  geom_text(stat='count', aes(label=..count..), hjust=2, color = 'white')+
  coord_flip()+
  theme_minimal(base_size = 16)

p2

read_pptx( '../Templates/CR.template.pptx' ) %>%
  add_slide   ( layout = 'TTE', master = 'CR') %>%
  ph_with     ( dml( print ( p1, newpage = F ) ), location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide   ( layout = 'TTE', master = 'CR') %>%
  ph_with     ( dml( print ( p2, newpage = F ) ), location = ph_location_type( type = "body" , id = 1) ) %>%
  print ( target = paste('Visits.Overview.', gsub(":","-", Sys.time()), ".pptx", sep="") )


# errors - for reporting later --------------------------------------------
# 
# list.ipv <- dt %>%
#   # filter(sjid == 4711) %>% filter(paramcd == 'w6m')
#   filter(!grepl('.iu', paramcd)) %>% 
#   ungroup %>% select  ( study, sjid, adt, avisitn, hpf ) %>%
#   group_by( study, sjid, adt, avisitn, hpf ) %>%
#   unique()  %>% 
#   group_by( study, sjid, adt, avisitn ) %>%
#   # filter(is.na(hpf)) %>% 
#   filter(n()>1) %>% 
#   left_join(.dd.FA('demo') %>% select(sjid, site))%>% 
#   arrange(sjid, adt, avisitn) 
# 
# dt %>% 
#   filter(!is.na(aval)) %>% 
#   filter(!grepl('.iu', paramcd)) %>% 
#   filter(sjid %in% list.ipv$sjid) %>% 
#   ungroup() %>% 
#   select(study, sjid, adt, avisitn, hpf, paramcd) %>%
#   right_join(list.ipv) %>% 
#   spread(paramcd, hpf) %>% 
#   arrange(adt) %>% 
#   as.data.frame()
#   filter( !(sjid %in% c(5035)) )
# 
# .rd.FACHILD('exttw')   %>% filter(PATNO == 'UF9')  %>% select(PATNO, EVENT_ID, C_INFODT, FORMPERF, HOWPERF) # wrong date?
# 
# .rd.FACHILD('tug')   %>% filter(PATNO == '4770') %>% select(PATNO, EVENT_ID, C_INFODT, FORMPERF, HOWPERF)
# .rd.FACHILD('faadl')   %>% filter(PATNO == '4770') %>% select(PATNO, EVENT_ID, C_INFODT, FORMPERF, HOWPERF)
# 
# .rd.FACHILD('exttw')   %>% filter(PATNO == 'UF12') %>% select(PATNO, EVENT_ID, C_INFODT, FORMPERF, HOWPERF)
# .rd.FACHILD('bbs')     %>% filter(PATNO == 'UF12') %>% select(PATNO, EVENT_ID, C_INFODT, FORMPERF, HOWPERF)
# 
# . end reporting part ----------------------------------------------------
