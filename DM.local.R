# setup -------------------------------------------------------------------

rm(list=ls())

require('labelled')
source('list.pars.R')

.vnames <- function ( df ) {
  df %<>%
    left_join(.rt('../DATA other/FACHILD.FACOMS.vnames.txt')) %>%
    mutate(vname = factor(vname, c('BL','6m','1y','18m','2y','3y','3y+'))) %>%
    select(study, vname, everything()) %>% 
    select(-avisit)
  return (df)
}

# data --------------------------------------------------------------------

dt. <- bind_rows(
  .dd.atx('scafi', c = T) %>% filter(!unable),
  # .dd.atx('pedsql', c = T),
  .dd.atx('adl'  , c = T),
  .dd.atx('bbs'  , c = T),
  .dd.atx('fars' , c = T)
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
  filter(sjid != 4637) # del exon 5 (null), no FARS data

table(dt.$avisitn, dt.$study)

# control age range -----------------------------------------------------------

dt. %<>% 
  .add.time ( keepadt = T  )

dt. %>% ungroup %>% filter(study == 'FACHILD') %>% select( age ) %>% 
  range()

dt. %<>% 
  filter(age < 21.2)

# FACHILD maximum minimal age is 17.9, remove non-eligible FACOMS patients

dt. %<>% 
  group_by(study, sjid) %>% 
  filter(min(age)<18) %>% 
  ungroup

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

# still some are NA -------------------------------------------------------
# scafis' remove there

dt. %<>% 
  filter(!is.na(aval))

# time variable -----------------------------------------------------------

dt. %<>%
  # group_by(study, sjid, amb) %>%
  group_by(study, sjid ) %>%
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

# changes and intervals ---------------------------------------------------

dt. %<>%
  left_join( base, by = c("study", "sjid", "paramcd") ) %>% 
  group_by ( study, sjid, paramcd ) %>% 
  arrange  ( study, sjid, paramcd, avisitn ) %>%
  mutate   ( cbl        = aval - bl ) %>% 
  mutate   ( window.dev = abs( time. - avisitn ) ) %>% 
  select( study, sjid, adt, avisitn, amb, age, time., paramcd, bl, aval, cbl, window.dev )

# follow-up characteristics for BL -----------------------------------------
# 12 patients that were unable at something at baseline. used as empty
# first age/dur during a visit is used (sometimes bbs comes from later)

dt.bl <- dt. %>%
  group_by ( study, sjid, avisitn ) %>% 
  mutate_at( vars('age', 'time.'), min ) %>% 
  select   ( study, sjid, avisitn, age, amb, paramcd, aval) %>%
  mutate_at( 'age' , round, 2) %>% 
  spread   ( paramcd , aval ) %>% 
  group_by ( study, sjid ) %>% 
  arrange  ( study, sjid, avisitn) %>% 
  mutate   ( 
    fu          = max(age)-min(age),
    visit.count = n(),
    bl.age      = min(age)
  ) %>%
  group_by( study, sjid ) %>% 
  filter( avisitn == min(avisitn) ) %>%
  ungroup

dm <- dt.bl %>% 
  # select(-pars.) %>% 
  left_join( demo. %>% select(-birthdt) ) %>% 
  mutate   ( amb = ifelse(amb == 'ambulatory', T, F)) %>% 
  select   ( -bl.age ) %>% 
  select   ( study, site, sjid, avisitn, sex, symp, gaa1, pm, age, fu, visit.count, everything() ) 

names(dm)
rm(demo., dt.bl, base)

# var_label(dt.bl) <- list(
#   study = 'Study',
#   symp = 'Age of Onset', pm = 'Point Mutation', gaa1 = 'Repeat Length (short)',
#   sex = 'Sex',
#   med.age = 'Age Group',
#   bl.age  = 'Age (BL)',
#   fu = 'Follow Up (years)',
#   visit.count = 'Visits'
#   )

# inclusion criteria ------------------------------------------------------
# done somewhere else
# dt.bl %<>% 
#   mutate( mFARS.20 = ifelse( mFARS  < 20, T, F ) ) %>% 
#   mutate( age.8    = ifelse( bl.age < 8 , T, F ) )
# 
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

# factor labels -----------------------------------------------------------

dt. %<>%
  # left_join(
  #   data.frame(paramcd = pars., param = params.)
  # ) %>% 
  mutate(paramcd = factor(paramcd, pars.)) %>%
  droplevels()

# visit names -------------------------------------------------------------

dt. %<>% 
  .vnames

# write -------------------------------------------------------------------

dm %>% 
  saveRDS ( 'DATA derived/dm.rds' )

dt. %>% 
  saveRDS ( 'DATA derived/dt.rds' )

rm(pars., params.)


# populations -------------------------------------------------------------

# fars.fu.times -----------------------------------------------------------

fars.tmp <- .dd.atx( 'fars', c = T ) %>%
  filter(!(sjid %in% sjids.FACHILD & study == 'FACOMS')) %>% 
  # filter(study == 'FACHILD') %>% 
  filter(paramcd == 'mFARS') %>% 
  select(study, sjid, avisit, adt, fpf, mFARS = aval)

dt.x <- dt. %>%
  ungroup %>% 
  select(sjid, vname, adt, avisitn) %>% 
  unique %>% 
  left_join(fars.tmp) %>% 
  mutate(mFARS = ifelse(is.na(mFARS), F, T))

rm(fars.tmp)

fu.tmp <- dt.x %>% 
  group_by(study, sjid) %>% 
  mutate( fu.time = as.numeric( max(adt)-min(adt) )/365.25) %>%
  filter( mFARS == T) %>% 
  mutate( fu.fars = as.numeric( max(adt)-min(adt) )/365.25) %>% 
  mutate( fu.fars = ifelse(sjid == 4851, 0, fu.fars ) ) %>% 
  select( sjid, fu.time, fu.fars ) %>% 
  unique

rm(dt.x)

dt.pop <- fu.tmp %>% 
  mutate(itt      = ifelse(sjid %in% c(5183, 4851, 4600, 4228, 4252) , F, T)) %>% 
  mutate(itt.1y   = ifelse(max(fu.fars) <= 0.75, F, itt )) %>%
  mutate(itt.2y   = ifelse(max(fu.fars) <= 1.75, F, itt )) %>%
  mutate(itt.3y   = ifelse(max(fu.fars) <= 2.50, F, itt )) %>%
  mutate(itt.3y.w = ifelse(itt.3y & (max(fu.fars) <= 3.50), T, F)) %>% #beware T/F the other way round
  select(sjid, starts_with('itt')) %>% 
  unique

rm(fu.tmp)

# FARS Subgroups ----------------------------------------------------------

dt.subs <- dm %>%
  mutate ( itt.FRE = ifelse( FARS.E > 8 , T, F )) %>% 
  mutate ( itt.FEx = ifelse( FARS.E > 8 & FARS.E < 32, T, F )) %>% 
  mutate ( itt.mFR = ifelse( mFARS  > 19, T, F )) %>% 
  select( sjid, starts_with('itt.'))


dt.pop %>% 
  left_join(dt.subs) %>% 
  .wds('DATA derived/FACHILD.pop')

