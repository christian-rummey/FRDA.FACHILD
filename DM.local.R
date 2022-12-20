
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

demo. <- .dd.FA('demo') %>% 
  filter( sjid %in% dt.$sjid ) %>% 
  filter( study %in% c('FACHILD', 'FACOMS')) %>% 
  mutate( study = ifelse(sjid %in% sjids.FACHILD, 'FACHILD', 'FACOMS')) %>% 
  select( study, site, sjid, sex, gaa1, symp, pm, birthdt )

# manual data changes -----------------------------------------------------
# remove unsuitable FACOMS pts 

dt. %<>% 
  filter(sjid != 4637) # severe pm (del exon 5, no FARS data)

# data entry errors 

dt. %<>% 
  mutate(adt = if_else(study == 'FACHILD' & sjid == 5136 & avisitn == 4, as.Date('2021-02-05'), adt)) %>% 
  mutate(adt = if_else(study == 'FACOMS'  & sjid == 54   & avisitn == 4, as.Date('2006-12-05'), adt))


# remove unable from timedM and add unable-pars --------------------------

# dt. %<>% 
#   mutate(paramcd = ifelse(paramcd == 'hpt.i', 'hpt.iu', paramcd)) %>% 
#   mutate(paramcd = ifelse(paramcd == 'w25.i', 'w25.iu', paramcd)) %>% 
#   bind_rows(
#     dt. %>% filter(paramcd %in% c('hpt.i', 'w25.i')) %>% filter(unable == F)
#   )

# adjust visit numbers from FACOMS to FACHILD version -------------------------

dt. <- dt. %>% 
  filter(study == 'FACOMS') %>% 
  filter(avisitn <= 4) %>% 
  mutate(avisitn =  ifelse(avisitn == 4, 6, avisitn)) %>% 
  mutate(avisitn =  ifelse(avisitn == 3, 5, avisitn)) %>% 
  mutate(avisitn =  ifelse(avisitn == 2, 3, avisitn)) %>% 
  # ungroup %>% select(avisitn) %>% table
  bind_rows(
    dt. %>% filter(study == 'FACHILD')
  ) %>% 
  ungroup

table(dt.$avisitn, dt.$study)

# fix alternating visit dates ---------------------------------------------
# code to check what happens
# dt. %>% 
#   # filter(study == 'FACHILD') %>%
#   # filter(sjid == 4752) %>%
#   # ggplot()+geom_point()+geom_line(aes(group = paramcd))+
#   # aes(x = (as.numeric(adt-min(adt))/365.25), y = paramcd, color = factor(avisitn))
#   # filter  ( !(sjid %in% c(4237, 4633, 4752)) ) %>%  # these are checked ok
#   # filter  ( !(sjid %in% c(107)) ) %>%   # not ok, but don't matter
#   group_by( study, sjid, avisitn ) %>% 
#   mutate  ( diff = min(adt)-adt ) %>% 
#   arrange ( -abs(diff ))

dt. %<>%
  group_by( study, sjid, avisitn ) %>%
  mutate  ( adt = min(adt) )

# control age range -----------------------------------------------------------

dt. %<>% 
  .add.time ( keepadt = T  )

dt. %>% ungroup %>% filter(study == 'FACHILD') %>% select( age ) %>% 
  range()

dt. %<>% 
  filter(age < 21.5)

# FACHILD maximum minimal age is 17.9
dt. %<>% 
  group_by(study, sjid) %>% 
  filter(min(age)<18) %>% 
  ungroup

# fix demo. ---------------------------------------------------------------

demo. <- dt. %>% 
  select(study, sjid) %>% 
  unique %>% 
  left_join(demo. %>% select(-study)) %>% 
  select(study, sjid, everything())

# factor labels -----------------------------------------------------------

dt. %<>% 
  mutate(paramcd = factor(paramcd, pars.)) 
# %>% 
#   mutate(param   = factor(paramcd, pars., labels = params.))

dt. %<>% 
  filter(sjid %in% demo.$sjid)

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

# save baseline (regardless of amb) --------------------------------------------

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

# this would only remove people without bl
# chg.all %<>% 
#   filter(!is.na(cbl))

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

dt. %<>% 
  # mutate(hpf = ifelse(is.na(hpf) | hpf == 'In-Person', 'In-Person', hpf)) %>%
  mutate(hpf = factor(hpf, c('Video', 'Audio Only', 'In-Person'))) 

# . -----------------------------------------------------------------------
# 12 patients that were unable at something at baseline. used as empty
# first age/dur during a visit is used (sometimes bbs comes from later)

dt.bl <- dt. %>% 
  group_by ( study, sjid ) %>% 
  select ( study, sjid, amb, avisitn, age, time., paramcd, aval) %>% 
  # select ( -c(param, bl, adt, unable, dev.y, fpf, hpf, cbl, -w25)) %>% 
  spread ( paramcd , aval ) %>% 
  arrange(study, sjid, avisitn) %>% 
  mutate( 
    fu     = max(age)-min(age),
    vc     = n(),
    bl.age = min(age)
  ) %>%
  group_by( study, sjid ) %>% 
  filter( avisitn == min(avisitn) ) %>%
  ungroup

# add subgroups and follow up stats ---------------------------------------

dt.bl %<>% 
  group_by(study) %>% 
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
  # select(-pars.) %>% 
  left_join( demo. %>% select(-birthdt) ) %>% 
  select   ( study, site, sjid, avisitn, sex, symp, gaa1, pm, everything() ) 

rm(demo.)

var_label(dt.bl) <- list(
  study = 'Study',
  symp = 'Age of Onset', pm = 'Point Mutation', gaa1 = 'Repeat Length (short)',
  sex = 'Sex',
  med.age = 'Age Group',
  bl.age  = 'Age (BL)',
  fu = 'Follow Up (years)',
  vc = 'Visits'
  )

# inclusion criteria ------------------------------------------------------

dt.group <- dt. %>% 
  group_by( study, sjid, paramcd ) %>% 
  filter( avisitn == min(avisitn) ) %>%
  filter( paramcd == 'mFARS') %>% 
  mutate( excl = ifelse( aval< 15, 'mFARS<20', 'ok' ) ) %>% 
  mutate( excl = ifelse( age < 8 , 'age<8'   , excl ) ) %>%
  ungroup %>% 
  select( study, sjid, excl )

# fix missing ones

dt.bl %<>% 
  left_join(dt.group) %>% 
  mutate( amb    = ifelse( sjid == 4844 , 'ambulatory'   , amb   )) %>% # no doubt. 
  mutate( excl   = ifelse( age  < 8     , 'age<8'        , excl  )) %>% 
  mutate( vis    = ifelse( vc   == 1    , 'no follow up' , 'ok'  )) 

# have one hierarchical one
# all -> excl.1 vis -> excl <8y -> mFARS < 20 -> excl non-amb. 
dt.bl %<>% 
  mutate( group  = excl ) %>% 
  mutate( group  = ifelse( vc == 1, 'no follow up', group)) %>% 
  mutate( group  = ifelse( group == 'ok' & amb != 'ambulatory', 'non-amb.', group )) %>%
  mutate( group = factor(group, c('no follow up', 'age<8', 'mFARS<20', 'non-amb.','ok')))
# %>% 
#   select( group ) %>% table

# write -------------------------------------------------------------------

dt.bl %>% 
  saveRDS ( 'DATA derived/dt.bl.rds' )

dt. %>% 
  saveRDS ( 'DATA derived/dt.long.rds' )

rm(pars., params.)
