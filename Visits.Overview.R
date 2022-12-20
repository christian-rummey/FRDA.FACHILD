
rm(list=ls())


# trying something new ----------------------------------------------------

branded_colors <- list(
  "blue"   = "#00798c",
  "red"    = "#d1495b",
  "yellow" = "#edae49",
  "green"  = "#66a182",
  "navy"   = "#2e4057", 
  "grey"   = "#8d96a3"
)

# read data ---------------------------------------------------------------

require('labelled')
source('list.pars.R')

dt.       <- readRDS('DATA derived/dt.long.rds')
dt.bl     <- readRDS('DATA derived/dt.bl.rds')
visstat   <- .ds.FACHILD('visstat')

# .rd.FACHILD('exttw') %>% filter(howperf != 1)
# .rd.FACHILD('bbs')   %>% filter(howperf != 1)
# .rd.FACHILD('t25fw') %>% filter(howperf != 1)
# .rd.FACHILD('tug')   %>% filter(howperf != 1)

# switches for output -----------------------------------------------------

# dt. %<>% 
#   left_join(dt.bl %>% select(study, sjid, vc)) %>% 
#   # filter(study == 'FACHILD') %>% 
#   filter(vc!=1) %>% 
#   droplevels()

# statistics about excluded individuals (one visit only) ------------------

dt. %>% 
  left_join(dt.bl %>% select(study, sjid, vc)) %>%
  ungroup %>% 
  filter(vc == 1) %>% 
  select(study, sjid, avisitn) %>% 
  unique() %>% 
  select(study) %>% table

# follow up type barchart ----------------------------

dt.$avisitn <- factor(dt.$avisitn)

levels(dt.$avisitn) <- c('BL','6m','1y','1.5y','2y', '3y')

dt <- dt. %>%
  ungroup %>% select  ( study, sjid, avisitn ) %>%
  unique() %>% 
  group_by(study, sjid) %>% 
  group_by( study ) %>%
  mutate(N   = length(unique(sjid))) %>% 
  group_by( study, sjid, avisitn ) %>%
  group_by( study, avisitn, N) %>% 
  count() %>% 
  mutate(pct = round(100*n/(N), 0)) %>% 
  left_join(
    data.frame(avisitn = levels(dt.$avisitn), time = c(0, 0.5,1, 1.5, 2, 3))
  )

p1 <- dt %>%
  filter(study == 'FACHILD') %>% 
  ggplot()+geom_col(position = position_dodge2(width = 1))+
  # aes(x = avisitn, y = pct) +
  aes(x = time, y = pct) +
  # aes(fill = study)+scale_fill_manual(values = c("#00798c","#edae49"))+
  scale_x_continuous(breaks = c(0,0.5,1, 1.5, 2, 3), labels = c('BL','6m','1y','1.5y','2y','3y'))+
  geom_text(aes(label = paste(n,'\n(',pct,'%)', sep = '')), nudge_y = -4, color = 'white')+
  # geom_text(aes(label = paste(n,'\n(',pct,'%)', sep = '')), color = 'black')+
  # coord_flip(ylim = c(0,112))+
  theme_minimal(base_size = 16)+
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank())+
  facet_wrap(~study, ncol = 1)+
  xlab('Follow Up Visit')+
  ylab('Subjects (%)')
p1 

# same from visstat -------------------------------------------------------
# 5138 BL is missing form visstat (V3 is listed as not done )

# other reasons
visstat %>% 
  filter(vismsrsn == 10) %>% 
  select(sjid, avisitn, adt, vismsrsn, vismscm)

dt2 <- visstat %>% 
  mutate(avisit = factor(avisitn, labels = c('BL','6m','1y','1.5y','2y','3y') ) ) %>% 
  mutate(vismsrsn. = case_when(
    vismsrsn == 10 ~ 'Other',
    vismsrsn == 4  ~ 'Subject did not respond',
    vismsrsn == 5  ~ 'Travel distance',
    vismsrsn == 6  ~ 'Medical problems',
    vismsrsn %in% c(1,2,3,14)  ~ 'Scheduling issues',
    vismsrsn == 15 ~ 'COVID-19 pandemic')) %>%
  mutate(vismsrsn. = factor(vismsrsn., c('COVID-19 pandemic','Scheduling issues',
                                         'Medical problems','Travel distance','Subject did not respond','Other'))) %>% 
  select(study , sjid, avisit, avisitn, viscmplt, assmperf, vismsrsn, vismsrsn.) %>% 
  group_by(avisit, vismsrsn.) %>% 
  count() %>% 
  mutate(pct = 100*n/108) %>% 
  mutate(tm.tmp = ifelse(avisit == 'BL', 0,
                         ifelse(avisit=='6m', 0.5,
                                ifelse(avisit=='1y', 1.0,
                                       ifelse(avisit=='1.5y', 1.5, 
                                              ifelse(avisit=='2y', 2,
                                                     ifelse(avisit=='3y', 3, NA)))))))

dt2 %>% 
  ungroup %>% 
  filter(!is.na(vismsrsn.)) %>% 
  summarise(sum(n))


dt2 %>% 
  group_by(vismsrsn.) %>% 
  filter(!is.na(vismsrsn.)) %>% 
  summarise(sum(n))

p2 <- dt2 %>% 
  ggplot()+geom_col()+
  # aes(x = avisitn, y = pct) +
  # aes(x = c(0, 0.5,1, 1.5, 2, 3), y = pct) +
  aes(x = tm.tmp, y = pct) +
  scale_x_continuous(breaks = c(0,0.5,1, 1.5, 2, 3), labels = c('BL','6m','1y','1.5y','2y','3y'))+
  aes(fill = vismsrsn. ) +
  # scale_x_continuous    ( breaks = seq(1,6,1), labels = c('BL','6m','1y','1.5y','2y','3y'))+
  scale_fill_brewer ( palette = 'Set1',na.value="grey")+
  theme_minimal(base_size = 16)+
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank())+
  xlab('Follow Up Visit')+
  ylab('Subjects (%)')+
  guides(fill = guide_legend("Reason"))+
  .leg_llh+
  theme(
    legend.position =  c(0.1, 0.1),
    legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid")
    )
  
p2  
  
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

p3 <- dt. %>%
  ungroup %>% 
  # mutate(hpf = ifelse( is.na(hpf), 'filler', as.character(hpf))) %>% 
  select( -fpf ) %>% 
  filter  ( !grepl('.iu', paramcd) ) %>% 
  select  ( study, sjid, avisitn, paramcd, hpf) %>% 
  group_by( study, sjid, avisitn, paramcd) %>%
  filter  ( hpf %in% c('Audio Only','Video')) %>% 
  ggplot()+geom_bar()+
  aes(x = paramcd)+
  geom_text(stat='count', aes(label=..count..), hjust=2, color = 'white')+
  coord_flip()+
  theme_minimal(base_size = 16)

p3

read_pptx( '../Templates/CR.template.pptx' ) %>%
  add_slide   ( layout = 'TTE', master = 'CR') %>%
  ph_with     ( dml( print ( p1, newpage = F ) ), location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide   ( layout = 'TTE', master = 'CR') %>%
  ph_with     ( dml( print ( p2, newpage = F ) ), location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide   ( layout = 'TTE', master = 'CR') %>%
  ph_with     ( dml( print ( p3, newpage = F ) ), location = ph_location_type( type = "body" , id = 1) ) %>%
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
