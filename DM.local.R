
# setup -------------------------------------------------------------------

rm(list=ls())

require('labelled')
source('list.pars.R')

dt. <- bind_rows(
  .dd.FA('fars' , c = T),
  .dd.FA('scafi', c = T) %>% filter(!(sjid == 5136 & paramcd == 'tug.i' & aval == 2)), # tug in two seconds
  .dd.FA('bbs'  , c = T),
  .dd.FA('adl'  , c = T),
  .dd.FA('pedsql', c = T),
  ) %>% 
  filter( study %in% c('FACHILD', 'FACOMS')) %>% 
  filter( paramcd %in% pars.) 

sjids.FACHILD <- dt. %>% 
  filter(study == 'FACHILD') %>%  select(sjid) %>% unique %>% deframe

dt. <- bind_rows(
    dt. %>% filter(study == 'FACHILD'),
    dt. %>% filter(study == 'FACOMS') %>% filter(!(sjid %in% sjids.FACHILD)) %>% filter(avisitn<=4)
    )

# remove unable from timedM and add unable-pars --------------------------

dt. %<>% 
  mutate(paramcd = ifelse(paramcd == 'hpt.i', 'hpt.iu', paramcd)) %>% 
  mutate(paramcd = ifelse(paramcd == 'w25.i', 'w25.iu', paramcd)) %>% 
  bind_rows(
    dt. %>% filter(paramcd %in% c('hpt.i', 'w25.i')) %>% filter(unable == F)
  )

# data entry errors -------------------------------------------------------

dt. %<>% 
  mutate(adt = if_else(study == 'FACHILD' & sjid == 5136 & avisitn == 4, as.Date('2021-02-05'), adt)) %>% 
  mutate(adt = if_else(study == 'FACOMS'  & sjid == 54   & avisitn == 4, as.Date('2006-12-05'), adt))

# adjust visit numbers from FACOMS to FACHILD order -------------------------

dt. <- dt. %>% 
  filter(study == 'FACOMS') %>% 
  filter(avisitn <= 4) %>% 
  mutate(avisitn =  ifelse(avisitn == 4, 6, avisitn)) %>% 
  mutate(avisitn =  ifelse(avisitn == 3, 5, avisitn)) %>% 
  mutate(avisitn =  ifelse(avisitn == 2, 3, avisitn)) %>% 
  # ungroup %>% select(avisitn) %>% table
  bind_rows(
    dt. %>% filter(study == 'FACHILD')
  )

table(dt.$avisitn, dt.$study)

# restrict ages -----------------------------------------------------------

dt. %<>% 
  .add.time(keepadt = T)

dt. %>% 
  ungroup %>% filter(study == 'FACHILD') %>%  
  select( age ) %>% 
  range()

dt. %<>% 
  filter(age < 21.5)

# FACHILD min age is 17.9
dt. %<>% 
  group_by(study, sjid) %>% 
  filter(min(age)<18) %>% 
  ungroup

# reduce patients ---------------------------------------------------------

# save fpf, hpf, unable (only from min date per visit)
dt.tmp <-  dt. %>% 
  group_by( study, sjid, avisitn ) %>% 
  mutate( adt = min(adt)) %>%
  mutate( age = min(age)) %>% 
  select( study, sjid, avisitn, adt, paramcd, fpf, hpf, unable) %>% 
  ungroup
  
dt. %<>% 
  select(-c(age, adt, fpf, hpf, unable)) %>% 
  filter(!is.na(aval))
  
dt. %<>% 
  spread(paramcd, aval) %>%
  # filter(is.na(mFARS)) %>%
  # filter(study == 'FACHILD') %>% group_by(sjid) %>% filter(n()>1)
  group_by(sjid) %>% 
  mutate(fu_v = n()) %>% 
  ungroup

dt. %>%
  group_by(study, fu_v) %>% 
  summarise(n=n()) %>% 
  spread(fu_v, n)

dt. %<>% 
  gather(paramcd, aval, pars.) %>% 
  filter(!is.na(aval))

dt. %<>% 
  left_join(dt.tmp) %>% 
  select(study, sjid, avisitn, adt, fpf, hpf, fu_v, paramcd, unable, aval)

# factor labels -----------------------------------------------------------

demo. <- .dd.FA('demo') %>% 
  filter( sjid %in% dt.$sjid ) %>% 
  filter( study %in% c('FACHILD', 'FACOMS')) %>% 
  mutate( study = ifelse(sjid %in% sjids.FACHILD, 'FACHILD', 'FACOMS'))

dt. %<>% 
  mutate(paramcd = factor(paramcd, pars.)) %>% 
  mutate(param   = factor(paramcd, pars., labels = params.))

dt. %<>% 
  filter(sjid %in% demo.$sjid)

rm(pars., params.)

# add steps ---------------------------------------------------------------
# 5183/BL has fars data in FACOMS, but not in FACHILD  
steps <- .dd.FA('steps', c = T) %>% 
  # filter(study == 'FACHILD') %>% 
  bind_rows(
    .dd.FA('steps') %>%
      filter(sjid == '5183') %>% mutate(study = 'FACHILD', avisitn = 3)
  ) %>% 
  select( study, sjid, avisitn, amb, w25 )

# keep visits without amb-status (baseline is available for everyone)

dt. %<>%
  left_join( steps )

dt. %<>%
  # filter(sjid == 4186) %>% 
  group_by(study, sjid, amb) %>%
  mutate( time. = as.numeric( adt - min(adt) ) / 365.25) %>%
  .add.time( tm = 'age', keepadt = T) %>% 
  # filter ( is.na(amb) ) %>% ### !!!
  group_by(study, sjid)

rm(steps)

# remove non-ambulatory t25fw ---------------------------------------------

# need to be kept
# dt. %>%
#   filter ((paramcd == 'w25.i' & amb == 'non-amb.')) %>%
#   filter (!(paramcd == 'w25.iu' & amb == 'non-amb.'))

# save baseline NOT by amb ----------------------------------------------------
# changed 26.03.2021

base <- dt. %>%
  # filter  ( !is.na(amb)) %>% # should affect no pne at BL
  group_by( study, sjid, paramcd ) %>% # baseline by-amb!!
  # filter  ( avisitn == min ( avisitn ) ) %>%
  filter  ( avisitn == 1 ) %>%
  group_by( study, sjid, paramcd) %>% 
  arrange ( study, sjid, paramcd) %>% 
  rename  ( bl = aval ) %>%
  ungroup %>% 
  select  ( study, sjid, paramcd, bl)


# calculate changes and intervals (not just for for 4 following visits) --------

chg.all <- dt. %>%
  # filter(!is.na(amb)) %>% 
  left_join( base, by = c("study", "sjid", "paramcd") ) %>% 
  group_by ( study, sjid, paramcd ) %>% 
  mutate( bl     = mean(bl    , na.rm=T)) %>% 
  # filter(!is.na(amb)) %>% 
  group_by( study, sjid, paramcd) %>% 
  arrange ( study, sjid, paramcd, avisitn ) %>%
  mutate  ( 
    # int   = age  - bl.age, 
    cbl   = aval - bl 
  ) %>% 
  mutate   ( dev.y = abs( time. - ((avisitn-1)/2) )) %>% 
  select( study, sjid, paramcd, avisitn, amb, cbl, dev.y)

chg.all %<>% 
  filter(!is.na(cbl))

# add changes to dt -------------------------------------------------------

dt. %<>% 
  left_join(base) %>% 
  # filter(is.na(bl))
  left_join(chg.all) %>%
  select( names(dt.), everything() ) %>% 
  # group_by( study, sjid, paramcd, avisitn ) %>% 
  # mutate( forslope = rank(int, ties.method = 'first')) %>% 
  # mutate( forslope = ifelse(forslope>1, NA, forslope)) %>% 
  group_by( study, sjid, paramcd, amb)

rm(chg.all, base)

dt.long <- dt.

dt.long %<>% 
  # mutate(hpf = ifelse(is.na(hpf) | hpf == 'In-Person', 'In-Person', hpf)) %>%
  mutate(hpf = factor(hpf, c('Video', 'Audio Only', 'In-Person'))) 

rm(dt.)
# . -----------------------------------------------------------------------
# 12 patients that were unable at something at baseline. used as empty
# first age/dur during a visit is used (sometimes bbs comes from later)

dt.bl <- dt.long %>% 
  group_by ( sjid ) %>% 
  select(-c(param, time., fu_v, bl, adt, unable, dev.y, fpf, hpf, cbl)) %>% 
  spread( paramcd , aval ) %>% 
  arrange(sjid, avisitn) %>% 
  mutate( 
    fu       = max(age)-min(age),
    fu_v     = max(avisitn)-1,
    bl.age   = min(age)
  ) %>%
  # filter(!(paramcd %in% c('FARS.Am', 'FARS.C'))) %>% 
  group_by( sjid ) %>% 
  filter( avisitn == min(avisitn) ) %>%
  ungroup

# dt.bl %>% group_by(sjid) %>% filter(n()>1) %>% arrange()

# add subgroups and follow up stats ---------------------------------------

dt.bl %<>% 
  # group_by(study) %>% 
  mutate(med.age = median(age)) %>% 
  mutate(med.FrE = median(FARS.E, na.rm=T)) %>% 
  mutate(med.age    = case_when(
    bl.age > med.age ~ paste('age >', round(med.age,1)),
    bl.age < med.age ~ paste('age <', round(med.age,1))
    )) %>% 
  mutate(med.FrE = case_when(
    FARS.E > med.FrE ~ paste('FARS.E >', round(med.FrE,1)),
    FARS.E < med.FrE ~ paste('FARS.E <', round(med.FrE,1)))
    )

dt.bl %<>% 
  left_join(demo.) %>% 
  mutate( pm = ifelse(pm == 0, 0, 1))

var_label(dt.bl) <- list(symp = 'Age of Onset', pm = 'Point Mutation', gaa1 = 'Repeat Length (short)',
                       med.age = 'Median Age',
                       sex = 'Sex',
                       bl.age  = 'Age (BL)', 
                       fu = 'Follow Up (years)',
                       fu_v = 'Follow Up (visits)')

# remove one visit only ---------------------------------------------------






# write -------------------------------------------------------------------

dt.bl %>% 
  saveRDS ( 'DATA derived/dt.bl.rds' )

dt.long %>% 
  saveRDS ( 'DATA derived/dt.long.rds' )
