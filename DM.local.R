
rm(list=ls())

parlst. <- c('Timed up and Go (1/s)'          , 'tug.i',
             'One Minute Walk (m)'            , 'w1m',
             '6 Minute Walk (m)'              , 'w6m',
             'Timed 25 Foot Walk (m/s)'       , 'w25.i',
             '9 Hole Peg Test (1/min)'        , 'hpt.i',
             'Timed 25 Foot Walk (m/s) unable', 'w25.iu',
             '9 Hole Peg Test (1/min) unable' , 'hpt.iu',
             'Berg Balance Scale'             , 'bbs',
             'Activities of Daily Living'     , 'ADL',
             'mFARS'                          , 'mFARS',
             'Upright Stability (FARS.E)'     , 'FARS.E',
             'Upper Limbs (FARS.B)'           , 'FARS.B',
             'Lower Limbs (FARS.C)'           , 'FARS.C',
             'Bulbar (FARS.Am)'               , 'FARS.Am' )

params. <- parlst.[c(seq(1,25,2))]
pars.   <- parlst.[c(seq(2,26,2))]

rm(parlst.)

dt. <- bind_rows(
  .dd.FA('fars' , c = T),
  .dd.FA('scafi', c = T) %>% filter(!(sjid == 5136 & paramcd == 'tug.i' & aval == 2)), # tug in two seconds
  .dd.FA('bbs'  , c = T),
  .dd.FA('adl'  , c = T)
  ) %>% 
  filter( paramcd %in% pars.) %>% 
  filter( study == 'FACHILD' )

# remove undable from timedM and add unable-pars --------------------------

dt. %<>% 
  mutate(paramcd = ifelse(paramcd == 'hpt.i', 'hpt.iu', paramcd)) %>% 
  mutate(paramcd = ifelse(paramcd == 'w25.i', 'w25.iu', paramcd)) %>% 
  bind_rows(
    dt. %>% filter(paramcd %in% c('hpt.i', 'w25.i')) %>% filter(unable == F)
  )

dt. %<>% 
  mutate(paramcd = factor(paramcd, pars.)) %>% 
  mutate(param   = factor(paramcd, pars., labels = params.))

rm(pars., params.)

# add steps ---------------------------------------------------------------
# 5183/BL has fars data in FACOMS, but not in FACHILD  
steps <- .dd.FA('steps', c = T) %>% 
  filter(study == 'FACHILD') %>% 
  bind_rows(
    .dd.FA('steps') %>%
      filter(sjid == '5183') %>% mutate(study = 'FACHILD', avisitn = 3)
  ) %>% 
  select( study, sjid, avisitn, amb, w25 )

# keep visits without amb-status (baseline is available for everyone)

dt. %<>%
  left_join( steps ) %>%
  .add.time( tm = 'age', keepadt = T) %>% 
  # filter ( is.na(amb) ) %>% ### !!!
  group_by(study, paramcd)

rm(steps)

# remove non-ambulatory t25fw ---------------------------------------------

dt. %<>% 
  filter (!(paramcd == 'w25.i' & amb == 'non-amb.')) %>% 
  filter (!(paramcd == 'w25.iu' & amb == 'non-amb.')) 

# save baseline by amb; only for covariate -------------------------------------

base <- dt. %>%
  filter  ( !is.na(amb)) %>%
  group_by( study, sjid, paramcd, amb) %>% # baseline by-amb!!
  filter  ( avisitn == min ( avisitn ) ) %>%
  group_by(sjid, paramcd, amb) %>% 
  arrange(sjid, paramcd, amb) %>% 
  rename  ( bl.age = age, bl = aval ) %>%
  ungroup %>% 
  select  ( study, sjid, avisitn, amb, paramcd, bl.age, bl)

# calculate changes and intervals (not just for for 4 following visits) --------

chg.all <- dt. %>%
  # filter(!is.na(amb)) %>% 
  left_join( base, by = c("study", "sjid", "avisitn", "paramcd") ) %>% 
  group_by ( sjid, paramcd ) %>% 
  mutate( bl     = mean(bl    , na.rm=T)) %>% 
  mutate( bl.age = mean(bl.age, na.rm=T)) %>% 
  # filter(!is.na(amb)) %>% 
  group_by( study, sjid, paramcd) %>% 
  arrange ( study, sjid, paramcd, avisitn ) %>%
  mutate  ( 
    # int   = age  - bl.age, 
    cbl   = aval - bl 
  ) %>% 
  mutate   ( dev.y = abs( time. - ((avisitn-1)/2) )) %>% 
  select( study, sjid, paramcd, avisitn, cbl, dev.y)

# add changes to dt -------------------------------------------------------

dt. %<>% 
  left_join(chg.all) %>%
  select( names(dt.), everything() ) %>% 
  # group_by( study, sjid, paramcd, avisitn ) %>% 
  # mutate( forslope = rank(int, ties.method = 'first')) %>% 
  # mutate( forslope = ifelse(forslope>1, NA, forslope)) %>% 
  group_by( study, sjid, paramcd, amb)

rm(chg.all, base)
# median BL age -----------------------------------------------------------

tmp <- dt. %>% 
  group_by(sjid) %>% 
  select(sjid, avisitn, age) %>% 
  filter( avisitn == 1 ) %>% 
  unique %>% group_by(sjid) %>% 
  filter(rank(age, ties.method = 'first')==1)

med.bl.age <- median(tmp$age)  

age.groups <- tmp %>% mutate(age.groups = ifelse(age<med.bl.age, '<13.8y', '>13.8y')) %>% select(-age)
rm(tmp)

dt. %<>%
  mutate ( age.groups = ifelse ( age < med.bl.age, '<13.8y', '>13.8y'))

rm(age.groups)

dt. %>% 
  saveRDS ( 'DATA derived/lmer.data.rds' )

