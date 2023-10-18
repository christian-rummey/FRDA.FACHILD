
source('project.settings.R')

# beware mFARS / NO mFARS ----------------------------------------------------
# when there is more than one visit.date, favor mFARS dates

dt. <- bind_rows(
  .dd('scafi' , c = T) %>% filter(!unable),
  .dd('pedsql', c = T),
  .dd('adl'   , c = T),
  .dd('bbs'   , c = T),
  .dd('fars'  , c = T)
) %>% 
  filter( study == 'FACHILD' ) %>% 
  .add.time( keepadt = T ) %>% 
  filter(!is.na(aval)) %>% 
  select(study, sjid, avisit, avisitn, adt, age, hpf) %>%
  mutate(hpf = ifelse(is.na(hpf), 'NA', hpf))

hpf.combined <- dt. %>% 
  group_by( sjid, avisitn) %>% select( hpf ) %>% 
  unique %>% 
  summarise_all( ~toString(na.omit(.)) )

dt. %<>% 
  select    ( study, sjid, avisit, avisitn, adt, age ) %>% 
  group_by  ( study, sjid, avisitn ) %>% 
  filter    ( rank (avisitn, ties.method = 'first') == 1 ) %>% 
  left_join ( hpf.combined ) %>% 
  left_join (.dd('demo') %>% select(sjid, site)) %>% 
  group_by  ( sjid ) %>% 
  mutate    ( startdt = min(adt))

# sort sjid by start date -------------------------------------------------

dt.$sjid <- fct_reorder(dt.$sjid, dt.$startdt, min)

.in.person <- c('1','1, NA','NA, 1','NA')
.mixed     <- c('1, NA, 3','NA, 1, 3')
.virtual   <- c('2','3','2, NA, 3','3, NA','NA, 2','NA, 3')

dt. %<>% ungroup %>% 
  mutate ( site = ifelse(site == 'Memphis', 'CHOP', site )) %>% 
  mutate ( hpf = ifelse( hpf %in% .in.person, 'In-Person', hpf )) %>% 
  mutate ( hpf = ifelse( hpf %in% .mixed    , 'Mixed'    , hpf )) %>% 
  mutate ( hpf = ifelse( hpf %in% .virtual  , 'Virtual'  , hpf )) %>% 
  # select ( hpf ) %>% table %>% 
  droplevels()

dt. %>% 
  ggplot()+  lemon::geom_pointline( shorten = .1 ) +
  aes(x = adt, y = sjid, color = site, shape = hpf )+
  aes(group = sjid)+
  scale_shape_manual( values = c(3,0,16) ) +
  facet_wrap(~study)+
  # theme_minimal(base_size = 16)+
  theme( panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank())+
  theme( axis.text.y = element_blank())+.leg_tl+
  xlab ( 'Date' )+ ylab('Subjects')+
  ggtitle('Visits Timeline')+theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept = as.Date('2020-03-01', linetype = 2, size = 5))+
  geom_vline(xintercept = as.Date('2019-10-08', linetype = 3))

# .sp()