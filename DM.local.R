# setup -------------------------------------------------------------------

rm(list=ls())
source('project.settings.R')

# data --------------------------------------------------------------------

dt. <- bind_rows(
  .dd.atx('scafi', c = T) %>% filter(!unable),
  # .dd.atx('pedsql', c = T),
  .dd.atx('adl'  , c = T),
  .dd.atx('bbs'  , c = T),
  .dd.atx('fars' , c = T)
  ) %>% 
  filter( study %in% c('FACHILD', 'FACOMS')) %>% 
  filter( paramcd %in% pars.) %>% 
  .add.time(keepadt = T) %>% 
  select(study, sjid, avisit, avisitn, adt, age, paramcd, aval)

sjids.FACHILD <- dt. %>% 
  filter(study == 'FACHILD') %>%  select(sjid) %>% unique %>% deframe

dt. <- bind_rows(
    dt. %>% filter(study == 'FACHILD'),
    dt. %>% filter(study == 'FACOMS') %>% filter(!(sjid %in% sjids.FACHILD)) %>% filter(avisitn<=3)
    )

# manual data adjustments -------------------------------------------------

dt. %<>% 
  filter(!is.na(aval)) %>% 
  filter(sjid != 4637) # del exon 5 (null), no FARS data

dt. %<>% # visit.dates seem mixed up. not sure I queried these; only changes age at these two visits
  mutate(adt = if_else(sjid == 4935 & avisitn == 0, as.Date('2018-06-28'), adt)) %>% 
  mutate(adt = if_else(sjid == 4935 & avisitn == 1, as.Date('2019-06-27'), adt)) %>%
  group_by(study, sjid) %>% 
  left_join(.dd.atx('demo') %>% select(sjid, birthdt)) %>% 
  mutate(age = as.numeric(adt-birthdt)/365.25 ) %>% 
  select(-birthdt)

dt. %<>% 
  filter(age>0)

# by alicia, should be removed in final data

dt. %<>% 
  filter(!(sjid == 4999 & avisit == 'V02')) %>% 
  filter(!(sjid == 4774 & avisit == 'V07'))

dt. %<>% 
  select(-avisit)

# control age range -----------------------------------------------------------

dt. %<>% 
  .add.time ( keepadt = T  )

dt. %>% ungroup %>% 
  filter(study == 'FACHILD') %>% select( age ) %>% 
  range()

dt. %<>% 
  filter(age < 21.2)

# FACHILD maximum minimal age is 17.9, remove non-eligible FACOMS patients

dt. %<>% 
  group_by(study, sjid) %>% 
  filter((min(age)<18)) %>% 
  ungroup

table(dt.$avisitn, dt.$study)

# demo --------------------------------------------------------------------

dm. <- .dd.atx('demo') %>% 
  filter( sjid %in% dt.$sjid ) %>% 
  filter( study %in% c('FACHILD', 'FACOMS')) %>% 
  mutate( study = ifelse(sjid %in% sjids.FACHILD, 'FACHILD', 'FACOMS')) %>% 
  select( study, site, sjid, sex, gaa1, symp, pm, birthdt )

# reduce demo. to FACOMS pts still in here 

dm. <- dt. %>% 
  select(study, sjid) %>% 
  unique %>% 
  left_join(dm. %>% select(-study)) %>% 
  select(study, sjid, everything())

dt. %<>% 
  filter(sjid %in% dm.$sjid)

# add steps ---------------------------------------------------------------
# only baseline!

dm. %<>% 
  left_join( 
    .dd.atx('steps', c = T) %>% 
      filter(avisitn==0) %>% 
      select(study, sjid, amb)
    ) %>% 
  filter(!is.na(amb))

# reduce also dt

dt. %<>% 
  filter(sjid %in% dm.$sjid)

# time variable -----------------------------------------------------------

dt. %<>%
  group_by(study, sjid ) %>%
  mutate( time. = age-min(age)) %>%
  select(study, sjid, avisitn, adt, age, time., paramcd, aval)

# Baseline Values ---------------------------------------------------------

base <- dt. %>%
  group_by( study, sjid, paramcd ) %>% 
  filter  ( avisitn == 0 ) %>% 
  group_by( study, sjid, paramcd) %>% 
  arrange ( study, sjid, paramcd) %>% 
  rename  ( bl = aval ) %>%
  ungroup %>% 
  select  ( study, sjid, paramcd, bl)

dm. %<>% 
  left_join(
    base %>% spread(paramcd, bl)
  )

# remove values when BL is missing :-(
# leave them in for now (so "missing values" stats work)

dt. %<>% 
  left_join(base)
# %>% 
#   filter(!is.na(bl))
rm(base)
# changes and intervals ---------------------------------------------------

dt. %<>%
  group_by ( study, sjid, paramcd ) %>% 
  arrange  ( study, sjid, paramcd, avisitn ) %>%
  mutate   ( cbl        = aval - bl ) %>% 
  mutate   ( window.dev = abs( time. - avisitn ) ) %>% 
  select  ( study, sjid, adt, avisitn, age, time., paramcd, bl, aval, cbl, window.dev )

# follow-up characteristics for BL -----------------------------------------
# 12 patients that were unable at something at baseline. used as empty
# first age/dur during a visit is used (sometimes bbs comes from later)

dt.fu <- dt. %>%
  group_by ( study, sjid, avisitn ) %>% 
  mutate_at( vars('age', 'time.'), min ) %>% 
  select   ( study, sjid, avisitn, age) %>%
  mutate_at( 'age' , round, 2) %>% 
  unique %>% 
  group_by ( study, sjid, age ) %>% 
  arrange  ( study, sjid, avisitn) %>% 
  mutate   ( 
    fu          = max(age)-min(age),
    visit.count = n(),
    bl.age      = min(age)
  ) %>%
  group_by( study, sjid ) %>% 
  filter( avisitn == min(avisitn) ) %>%
  ungroup

dm. <- dt.fu %>% 
  left_join( dm. %>% select(-birthdt) ) %>% 
  # select   ( -bl.age ) %>% 
  select   ( study, site, sjid, avisitn, sex, symp, gaa1, pm, bl.age, fu, visit.count, everything() ) 

# names(dm.)
rm(dt.fu)

# var_label(dt.bl) <- list(
#   study = 'Study',
#   symp = 'Age of Onset', pm = 'Point Mutation', gaa1 = 'Repeat Length (short)',
#   sex = 'Sex',
#   med.age = 'Age Group',
#   bl.age  = 'Age (BL)',
#   fu = 'Follow Up (years)',
#   visit.count = 'Visits'
#   )

# factor labels -----------------------------------------------------------

dt. %<>%
  mutate(paramcd = factor(paramcd, pars.)) %>%
  droplevels()

# write -------------------------------------------------------------------

dm. %>% 
  saveRDS ( 'DATA derived/dm.rds' )

dt. %>% 
  saveRDS ( 'DATA derived/dt.rds' )

rm(pars., params.)


# populations -------------------------------------------------------------

# fars.fu.times -----------------------------------------------------------
# 
# fars.tmp <- .dd.atx( 'fars', c = T ) %>%
#   filter(!(sjid %in% sjids.FACHILD & study == 'FACOMS')) %>% 
#   # filter(study == 'FACHILD') %>% 
#   filter(paramcd == 'mFARS') %>% 
#   select(study, sjid, avisit, adt, fpf, mFARS = aval)
# 
# dt.x <- dt. %>%
#   ungroup %>% 
#   select(sjid, vname, adt, avisitn) %>% 
#   unique %>% 
#   left_join(fars.tmp) %>% 
#   mutate(mFARS = ifelse(is.na(mFARS), F, T))
# 
# rm(fars.tmp)
# 
# fu.tmp <- dt.x %>% 
#   group_by(study, sjid) %>% 
#   mutate( fu.time = as.numeric( max(adt)-min(adt) )/365.25) %>%
#   filter( mFARS == T) %>% 
#   mutate( fu.fars = as.numeric( max(adt)-min(adt) )/365.25) %>% 
#   mutate( fu.fars = ifelse(sjid == 4851, 0, fu.fars ) ) %>% 
#   select( sjid, fu.time, fu.fars ) %>% 
#   unique
# 
# rm(dt.x)
# 
# dt.pop <- fu.tmp %>% 
#   mutate(itt      = ifelse(sjid %in% c(5183, 4851, 4600, 4228, 4252) , F, T)) %>% 
#   mutate(itt.1y   = ifelse(max(fu.fars) <= 0.75, F, itt )) %>%
#   mutate(itt.2y   = ifelse(max(fu.fars) <= 1.75, F, itt )) %>%
#   mutate(itt.3y   = ifelse(max(fu.fars) <= 2.50, F, itt )) %>%
#   mutate(itt.3y.w = ifelse(itt.3y & (max(fu.fars) <= 3.50), T, F)) %>% #beware T/F the other way round
#   select(sjid, starts_with('itt')) %>% 
#   unique
# 
# rm(fu.tmp)
# 
# # FARS Subgroups ----------------------------------------------------------
# 
# dt.subs <- dm %>%
#   mutate ( itt.FRE = ifelse( FARS.E > 8 , T, F )) %>% 
#   mutate ( itt.FEx = ifelse( FARS.E > 8 & FARS.E < 32, T, F )) %>% 
#   mutate ( itt.mFR = ifelse( mFARS  > 19, T, F )) %>% 
#   select( sjid, starts_with('itt.'))
# 
# 
# dt.pop %>% 
#   left_join(dt.subs) %>% 
#   .wds('DATA derived/FACHILD.pop')
# 
