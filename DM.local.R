# setup -------------------------------------------------------------------

rm(list=ls())

require('labelled')
source('list.pars.R')

dt. <- bind_rows(
  .dd.atx('fars' , c = T),
  # .dd.atx('scafi', c = T) %>% filter(study == 'FACHILD'), # %>% filter(!(sjid == 5136 & paramcd == 'tug.i' & aval == 2)), # tug in two seconds
  # .dd.atx('pedsql', c = T),
  # .dd.atx('adl'  , c = T),
  .dd.atx('bbs'  , c = T)
  ) %>% 
  filter( study %in% c('FACHILD', 'FACOMS')) %>% 
  filter( paramcd %in% pars.) 

dt. %<>% 
  select(-fpf, -hpf)

sjids.FACHILD <- dt. %>% 
  filter(study == 'FACHILD') %>%  select(sjid) %>% unique %>% deframe

dt. <- bind_rows(
    dt. %>% filter(study == 'FACHILD'),
    dt. %>% filter(study == 'FACOMS') %>% filter(!(sjid %in% sjids.FACHILD)) %>% filter(avisitn<=3)
    )

demo. <- .dd.atx('demo') %>% 
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
  mutate(adt = if_else(study == 'FACHILD' & sjid == 5136 & avisitn == 4, as.Date('2021-02-05'), adt))

table(dt.$avisitn, dt.$study)

# control age range -----------------------------------------------------------

dt. %<>% 
  .add.time ( keepadt = T  )

dt. %>% ungroup %>% filter(study == 'FACHILD') %>% select( age ) %>% 
  range()

dt. %<>% 
  filter(age < 21.2)

# FACHILD maximum minimal age is 17.9
# removes non-eligible FACOMS patients

dt. %<>% 
  group_by(study, sjid) %>% 
  filter(min(age)<18) %>% 
  ungroup

# remove double-entry visits ----------------------------------------------
# none?

dt. %>% 
  group_by(study, sjid, avisitn, paramcd) %>% 
  filter  ( n()!=1 )

# reduce demo. to FACOMS pts still in here---------------------------------

demo. <- dt. %>% 
  select(study, sjid) %>% 
  unique %>% 
  left_join(demo. %>% select(-study)) %>% 
  select(study, sjid, everything())

dt. %<>% 
  filter(sjid %in% demo.$sjid)

# add steps ---------------------------------------------------------------

dt. %<>%
  left_join( 
    .dd.atx('steps', c = T) %>% filter(!is.na(adt)) %>% select(study, sjid, avisitn, fds.act, amb)
    ) 

# time variable -----------------------------------------------------------

dt. %<>%
  group_by(study, sjid, amb) %>%
  mutate( time. = as.numeric( adt - min(adt) ) / 365.25) %>%
  # .add.time( tm = 'age', keepadt = T) %>% 
  # filter ( is.na(amb) ) %>% ### !!!%>% 
  select(study, sjid, adt, avisitn, amb, age, time., paramcd, aval)

# save baseline (regardless of amb) --------------------------------------------

base <- dt. %>%
  # filter  ( !is.na(amb)) %>% # should affect no pne at BL
  group_by( study, sjid, paramcd ) %>% # baseline by-amb!!
  # filter  ( avisitn == min ( avisitn ) ) %>%
  filter  ( avisitn == 0 ) %>%
  group_by( study, sjid, paramcd) %>% 
  arrange ( study, sjid, paramcd) %>% 
  rename  ( bl = aval ) %>%
  ungroup %>% 
  select  ( study, sjid, paramcd, bl)

# calculate changes and intervals (not just for 4 following visits) -------

dt. %<>%
  left_join( base, by = c("study", "sjid", "paramcd") ) %>% 
  group_by ( study, sjid, paramcd ) %>% 
  arrange  ( study, sjid, paramcd, avisitn ) %>%
  mutate   ( 
    # int   = age  - bl.age, 
    cbl   = aval - bl 
  ) %>% 
  mutate   ( dev.y = abs( time. - avisitn ) ) %>% 
  select( study, sjid, adt, avisitn, amb, age, time., paramcd, bl, aval, cbl, dev.y )

# dt. %<>% 
#   # mutate(hpf = ifelse(is.na(hpf) | hpf == 'In-Person', 'In-Person', hpf)) %>%
#   mutate(hpf = factor(hpf, c('Video', 'Audio Only', 'In-Person'))) 

# follow-up characteristics for BL -----------------------------------------
# 12 patients that were unable at something at baseline. used as empty
# first age/dur during a visit is used (sometimes bbs comes from later)

dt.bl <- dt. %>%
  # filter ( sjid == 4566 )  %>%
  group_by ( study, sjid, avisitn ) %>% 
  mutate_at( vars('age', 'time.'), min ) %>% 
  # filter   ( time. == min(time.) ) %>% 
  select ( study, sjid, avisitn, age, amb, paramcd, aval) %>%
  mutate_at( 'age' , round, 2) %>% 
  spread ( paramcd , aval ) %>% 
  group_by ( study, sjid ) %>% 
  arrange( study, sjid, avisitn) %>% 
  mutate( 
    fu          = max(age)-min(age),
    visit.count = n(),
    bl.age      = min(age)
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
  visit.count = 'Visits'
  )

# inclusion criteria ------------------------------------------------------

dt.bl %<>% 
  mutate( mFARS.20 = ifelse( mFARS  < 20, T, F ) ) %>% 
  mutate( age.8    = ifelse( bl.age < 8 , T, F ) )

# # fix missing ones
# 
# dt.bl %>% 
#   mutate( excl   = ifelse( age  < 8     , 'age<8'        , excl  )) %>% 
#   mutate( vis    = ifelse( vc   == 1    , 'no follow up' , 'ok'  )) 

# # have a hierarchical inclusion? -----------------------------------------------------
# # all -> excl.1 vis -> excl <8y -> mFARS < 20 -> excl non-amb. 
# 
# dt.bl %>% 
#   mutate( group  = excl ) %>% 
#   mutate( group  = ifelse( vc == 1, 'no follow up', group)) %>% 
#   mutate( group  = ifelse( group == 'ok' & amb != 'ambulatory', 'non-amb.', group )) %>%
#   mutate( group = factor(group, c('no follow up', 'age<8', 'mFARS<20', 'non-amb.','ok')))
# %>% 
#   select( group ) %>% table

dt. %>% 
  left_join(.rt('../DATA other/FACHILD.FACOMS.vnames.txt')) %>% 
  mutate(vname = factor(vname, c('BL','6m','1y','18m','2y','3y','4y'))) %>% 
  select(-avisit) 

# write -------------------------------------------------------------------

dt.bl %>% 
  saveRDS ( 'DATA derived/dt.bl.rds' )

dt. %>% 
  saveRDS ( 'DATA derived/dt.long.rds' )

rm(pars., params.)



# # factor labels -----------------------------------------------------------
# 
# dt. %<>% 
#   mutate(paramcd = factor(paramcd, pars.)) %>% 
#   droplevels()
# 
# dt. %<>% 
#   left_join(
#     data.frame(paramcd = pars., param = params.)
#   )
# 
# 

# visit names -------------------------------------------------------------

