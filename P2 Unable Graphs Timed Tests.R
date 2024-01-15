
rm(list=ls())
.mycols <- c('#e41a1c','#457EB7','black')

dm. <- readRDS('DATA derived/dm.rds')
source('project.settings.R')

# extended walking --------------------------------------------------------

exttw. <- .ds.FACHILD( 'exttw' ) %>%
  mutate_at(vars('adext', 'cpexm1t1', 'cpexm1t2','cpexm6'), as.numeric) %>% 
  rename( w1m.1  = extm1t1 ) %>% 
  rename( w1m.2  = extm1t2 ) %>% 
  rename( w6m    = extm6 ) %>% 
  ungroup 

value_map <- c(
  "1" = "unable",
  "2" = "time limit",
  "3" = "fatigue",
  "4" = "refused"
)

value_map_dev <- c(
  "0" = "none",
  "1" = "Cane",
  "2" = "Canes/crutches",
  "3" = "Walker",
  "4" = "Canine assistance"
)

exttw. %<>% 
  # select(study, sjid, avisitn, avisitn, adt, adext, cpexm1t1, cpexm1t2, cpexm6) %>% 
  mutate( cpexm1t1 = ifelse(!is.na(cpexm1t1), as.character(    value_map[as.character(cpexm1t1)]), NA),
          cpexm1t2 = ifelse(!is.na(cpexm1t2), as.character(    value_map[as.character(cpexm1t2)]), NA),
          cpexm6   = ifelse(!is.na(cpexm6  ), as.character(    value_map[as.character(cpexm6)]), NA),
          adext    = ifelse(!is.na(adext   ), as.character(value_map_dev[as.character(adext)]), NA)
  )

exttw.cat <- exttw. %>% 
  # mutate( cpexm1t1 = ifelse(!is.na(cpexm1t1), cpexm1t1, 'ok' ) ) %>% 
  # mutate( cpexm1t2 = ifelse(!is.na(cpexm1t2), cpexm1t2, 'ok' ) ) %>% 
  # mutate( cpexm1t2 = factor( cpexm1t2, c('time limit', 'fatigue','refused','unable'))) %>% 
  # mutate( cpexm6   = ifelse(!is.na(cpexm6), cpexm6, 'ok' ) ) %>% 
  # mutate( cpexm6   = factor( cpexm6, c('time limit', 'fatigue','refused','unable'))) %>% 
  select( study, sjid, avisitn, adt, avisit, adext, cpexm1t1,cpexm1t2,cpexm6 ) %>% 
  gather( test, val, cpexm1t1, cpexm1t2, cpexm6 ) %>% 
  rename( device = adext)

# t25fw -------------------------------------------------------------------

t25fw. <- bind_rows(
  .ds.FACOMS  ('t25fw') %>% mutate_at(vars('cptt25','adt251','t251sc','t252sc','cpt25t2','cpt25t1'), as.numeric),
  .ds.FACHILD ('t25fw') %>% mutate_at(vars(         'adt251','t251sc','t252sc','cpt25t2','cpt25t1'), as.numeric)
) %>% 
  # mutate( fpf = ifelse((!is.na(t251sc ) | !is.na(t252sc)) & fpf == 'No', 'Yes', fpf)) %>% 
  # mutate( fpf = ifelse((!is.na(cpt25t2) | !is.na(cpt25t1)) & fpf == 'No', 'Yes', fpf)) %>% 
  select(-hpf, -fpf) %>% 
  group_by(study, sjid, avisitn, adt) %>% 
  rename( w25.1     = t251sc ) %>% 
  rename( w25.2     = t252sc ) %>% 
  ungroup

# this seems to work for all these cases
# t25fw. %>%
#   mutate( cptt25. = ifelse(is.na(cptt25), pmin( cpt25t1, cpt25t2, na.rm=T), cptt25) ) %>% 
#   # mutate( cptt25. = pmin( cpt25t1, cpt25t2, na.rm=T)) %>% 
#   # filter(is.na(cpt25t1) & !is.na(cpt25t2) ) %>%
#   # filter(!is.na(cpt25t1) & is.na(cpt25t2) ) %>%
#   filter(cpt25t1 != cpt25t2)

t25fw. %<>%
  mutate( cptt25 = ifelse(is.na(cptt25), pmin( cpt25t1, cpt25t2, na.rm=T), cptt25) )
  # %>%
#   select( -c( cpt25t1,  cpt25t2 ) )

value_map <- c(
  "1" = "unable",
  "2" = "unable (unrel.)",
  "3" = "fatigue",
  "4" = "refused"
)

value_map_dev <- c(
  "0" = "none",
  "1" = "Cane",
  "2" = "Canes/crutches",
  "3" = "Walker",
  "4" = "Canine assistance"
)

t25fw. %<>% 
  mutate( cptt25  = ifelse( !is.na(cptt25) , as.character(value_map[as.character(cptt25)]), NA),
          cpt25t1 = ifelse( !is.na(cpt25t1), as.character(value_map[as.character(cpt25t1)]), NA),
          cpt25t2 = ifelse( !is.na(cpt25t2), as.character(value_map[as.character(cpt25t2)]), NA),
          adt251  = ifelse( !is.na(adt251) , as.character(value_map_dev[as.character(adt251)]), NA)
  ) 

t25fw.cat <- t25fw. %>%
  select(-w25.1, -w25.2) %>% 
  gather( test, val, cptt25, cpt25t1, cpt25t2 ) %>% 
  rename( device = adt251 )


# 9hpt -------------------------------------------------------------------

f9hpt. <- bind_rows(
  .ds.FACOMS ('f9hpt'   ) %>% mutate_at(vars('cpt9hpt', 'cptdomt1', 'cptdomt2', 'cptndmt1', 'cptndmt2'), as.numeric), 
  .ds.FACHILD('f9hpt01' ) %>% mutate_at(vars(           'cptdomt1','cptdomt2','cptndmt1','cptndmt2'), as.numeric)
) %>% 
  rename(
    hpt.d1  = domt1,
    hpt.d2  = domt2,
    hpt.n1  = ndomt1,
    hpt.n2  = ndomt2
  ) %>%
  # filter(rank(adt, ties.method = 'first') == 1) %>%
  ungroup

# this seems to work for all these cases
# f9hpt. %>%
#   select(-c(hpf, fpf, domhand)) %>% 
#   mutate( cptdomt = ifelse(is.na(cpt9hpt), pmax( cptdomt1, cptdomt2, na.rm=T), cpt9hpt) ) %>%
#   mutate( cptndmt = ifelse(is.na(cpt9hpt), pmax( cptndmt1, cptndmt2, na.rm=T), cpt9hpt) ) %>%
#   # mutate( cptdomt. = pmin( cpt25t1, cpt25t2, na.rm=T)) %>%
#   # filter( is.na(cptdomt1) & !is.na(cptdomt2) ) %>%
#   # filter(!is.na(cptdomt1) &  is.na(cptdomt2) ) %>%
#   filter(cptndmt1 != cptndmt2)

f9hpt. %<>%
  mutate( cptdomt = ifelse(is.na(cpt9hpt), pmax( cptdomt1, cptdomt2, na.rm=T), cpt9hpt) ) %>%
  mutate( cptndmt = ifelse(is.na(cpt9hpt), pmax( cptndmt1, cptndmt2, na.rm=T), cpt9hpt) ) %>%
  select( -c( hpf,  fpf, domhand ) ) %>% 
  select( -c( cptdomt1, cptdomt2, cptndmt1, cptndmt2 ) )

value_map <- c(
  "1" = "unable",
  "2" = "time limit",
  "3" = "fatigue",
  "4" = "refused"
)

f9hpt. %<>%
  mutate( cptdomt = ifelse(!is.na(cptdomt), as.character(value_map[as.character(cptdomt)]), NA),
          cptndmt = ifelse(!is.na(cptndmt), as.character(value_map[as.character(cptndmt)]), NA)
          )

f9hpt.cat <- f9hpt. %>%
  # filter(is.na(cptdomt) & !is.na(cpt9hpt))
  select( -c( 'hpt.d1', 'hpt.d2', 'hpt.n1', 'hpt.n2') ) %>% 
  select( -cpt9hpt ) %>%
  gather( test, val, cptdomt, cptndmt )

# tug -------------------------------------------------------------------

tug. <- bind_rows(
  .ds.FACHILD('tug' ) %>% mutate_at(vars( 'adtug','cptug' ), as.numeric)
) %>% 
  select(-c(hpf, fpf)) %>% 
  ungroup

value_map <- c(
  "1" = "unable",
  "2" = "unable (unrel.)",
  "3" = "fatigue",
  "4" = "refused"
)

value_map_dev <- c(
  "0" = "none",
  "1" = "Cane",
  "2" = "Canes/crutches",
  "3" = "Walker",
  "4" = "Canine assistance"
)

tug. %<>%
  mutate( cptug = ifelse(!is.na(cptug), as.character(    value_map[as.character(cptug)]), NA),
          adtug = ifelse(!is.na(adtug), as.character(value_map_dev[as.character(adtug)]), NA)
          )

tug.cat <- tug. %>%
  select( -c( 'tug') ) %>% 
  gather( test, val, cptug ) %>% 
  rename( device = adtug )

# combine_data -----------------------------------------------------------------

dt. <- bind_rows(
  exttw.cat,
  t25fw.cat,
  f9hpt.cat,
  tug.cat
  ) %>% 
  left_join(
    readRDS('DATA derived/dm.rds') %>% select(sjid, bl.age, fu, itt, bl.amb, starts_with('subg'))
    ) %>% 
  filter(itt) %>%
  # select(val) %>% table
  mutate( val = factor( val, c('time limit', 'fatigue','refused','unable (unrel.)','unable')))

rm(exttw.cat, t25fw.cat, f9hpt.cat, tug.cat,
   exttw., f9hpt., t25fw., tug.)

# make sure every sjid is used only in ONE study --------------------------

dt. %<>%
  select(-itt) %>%
  left_join(
    dm. %>% select(study, sjid, itt)
  ) %>%
  filter(itt)

# unable graph ------------------------------------------------------------

dt.sum <- dt. %>%
  group_by ( study, avisitn, test ) %>% 
  mutate   ( N = n() ) %>% 
  group_by ( study, avisitn, test, N, val ) %>% 
  filter   ( !is.na(val) ) %>%  
  summarise( n = n() ) %>% 
  mutate   ( pct = 100*n/N) %>% 
  filter   ( avisitn %in% c(0,1,2,3))

dt.sum %>%
  mutate   ( test = factor(test, 
                           levels = c('cptdomt','cptndmt','cptt25','cptug','cpexm1t1','cpexm1t2','cpexm6'), 
                           labels = c('9HPT (dom.)','9HPT (nondom.)','T25FW','Timed Up & Go','One Minute Walk 1','One Minute Walk 2','Six Minute Walk'))) %>% 
  filter   ( test %in% c('T25FW','One Minute Walk 1','One Minute Walk 2','Six Minute Walk')) %>% 
  ggplot()+geom_col()+
  aes(x = avisitn, y = pct)+
  aes(fill = val)+
  scale_fill_manual(values = c('#bae4b3','#31a354','#006d2c','#a6bddb','#3182bd'))+
  # facet_wrap(study~test, ncol = 7)+
  facet_wrap(study~test, ncol = 4)+
  geom_hline(yintercept = c(0,50,100), linetype = 3)+
  .leg_lr+
  xlab('Visit')+
  ylab('Percentage')
   
# device graph ------------------------------------------------------------

dt.sum <- dt. %>% 
  filter   ( test %in% c( 'cptt25','cptug','cpexm1t1','cpexm1t2','cpexm6' )) %>% 
  mutate   ( test = ifelse ( test %in% c('cptt25','cptug'), test, 'Extended Walking' )) %>% 
  mutate   ( test = factor(test, 
                           levels = c('cptt25','cptug','Extended Walking'), 
                           labels = c('T25FW','Timed Up & Go','Extended Walking'))) %>% 
  select   ( study, sjid, avisitn, adt, avisitn, test, device ) %>%
  unique   ( ) %>% 
  mutate   ( device = factor(device, levels = c('none','Cane','Canes/crutches', 'Canine assistance', 'Walker'))) %>% 
  group_by ( study, avisitn, test ) %>% 
  mutate   ( N = n() ) %>% 
  group_by ( study, avisitn, test, N, device ) %>% 
  # filter   ( val != val) ) %>%  
  summarise( n = n() ) %>% 
  mutate   ( pct = 100*n/N) %>% 
  filter   ( avisitn %in% c(0,1,2,3))

dt.sum %>%
  filter(!is.na(device)) %>%
  filter(device != 'none') %>% # NA is uncluded in this line, and in stats ("no device")
  ggplot()+geom_col()+
  aes(x = avisitn, y = pct)+
  aes(fill = device)+
  scale_fill_manual(values = c('#c7e9c0','#a1d99b','#74c476','#238b45','grey'))+
  facet_wrap(study~test, ncol = 3)+
  geom_hline(yintercept = c(0,50,100), linetype = 3)+
  .leg_lr+
  xlab('Visit')+
  ylab('Percentage')



