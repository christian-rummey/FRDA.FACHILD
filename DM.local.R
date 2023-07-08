# setup -------------------------------------------------------------------

rm(list=ls())
source('project.settings.R')

pars. <- c(pars.)#, .l.FARS.E[c(2,3,4,5,6,7,9)])

# data --------------------------------------------------------------------

dt. <- bind_rows(
  .dd('scafi' , c = T) %>% filter(!unable),
  .dd('pedsql', c = T),
  .dd('adl'   , c = T),
  .dd('bbs'   , c = T),
  .dd('fars'  , c = T)
  ) %>% 
  filter( study %in% c('FACHILD', 'FACOMS')) %>% 
  filter( paramcd %in% pars.) %>% 
  .add.time( keepadt = T ) %>% 
  select(study, sjid, avisit, avisitn, adt, age, paramcd, aval)

sjids.FACHILD <- .rd.FACHILD('demo') %>% 
  select(sjid) %>% deframe

dt. <- bind_rows(
    dt. %>% filter(study == 'FACHILD'),
    dt. %>% filter(study == 'FACOMS' ) %>% filter(!(sjid %in% sjids.FACHILD)) %>% filter(avisitn<=3)
    )

# control age range -----------------------------------------------------------

dt. %<>% 
  .add.time ( keepadt = T  )

dt. %>% ungroup %>% 
  filter(study == 'FACHILD') %>% select( age ) %>% 
  range()

dt. %<>% 
  filter(age < 21.2)

# FACHILD maximum BL age is 17.9, remove non-eligible FACOMS patients

dt. %<>% 
  group_by(study, sjid) %>% 
  filter( min(age)<18 ) %>% 
  ungroup

# ITT Definitions ---------------------------------------------------------
# ITT includes both studies, with same definitions
# pts from FACHILD that miss any criteria will be in datasets, but not in ITT
# 1) fit age range, 2) BL mFARS >= 17, 3) n(fars)>1

sjids.itt <- dt. %>% select(sjid) %>% unique() %>% deframe
  
# FACOMS/FACHILD with < 2 visitS 
exclude.mFARS.n1 <- dt. %>% filter(paramcd == 'mFARS' ) %>% group_by(sjid) %>% filter(n()==1) %>% select(sjid) %>% deframe
exclude.mFARS.17 <- dt. %>% filter(paramcd == 'mFARS' & avisit == 'BL' & aval < 17) %>% select(study, sjid) %>% deframe

# FACOMS with < 1 visit # this also removes 4851 (no bl) from itt, and 4787, 4866, 4882; 
# these are 17-20 and stay in: 4916, 4928, 4934, 4942, 4998
# see "FACHILD Baseline Criteria.R"

sjids.itt <- discard(sjids.itt, ~.x %in% c(exclude.mFARS.n1, exclude.mFARS.17)) # missing FACHILD BL visit
sjids.itt <- discard(sjids.itt, ~.x == 4851) # missing FACHILD BL visit
sjids.itt <- discard(sjids.itt, ~.x ==   80) # first fars visit != BL

# datasets will have "all enrolled" for FACHILD, but only pts with 2 FARS FU for FACOMS --------

dt. %<>% 
  filter( (sjid %in% sjids.itt & study == 'FACOMS') | study == 'FACHILD' )

sjids.FACOMS <- dt. %>% 
  filter( study == 'FACOMS' & paramcd == 'mFARS') %>% 
  group_by( sjid ) %>% 
  filter( n()>1 ) %>%
  filter( min(avisitn) == 0 ) %>% 
  select( sjid ) %>% deframe() %>% unique()

dt. %>% 
  filter(paramcd == 'mFARS') %>% 
  group_by(sjid) %>% 
  filter(min(avisitn) == 0) %>%
  filter(sjid %in% c(sjids.FACOMS, sjids.FACHILD)) %>% 
  ungroup %>% select(study, avisitn) %>% table

sjids.itt <- tibble(sjids.itt) %>% filter(sjids.itt %in% c( sjids.FACOMS, sjids.FACHILD )) %>% deframe

# now all is define, reduce
dt. %<>% 
  filter(sjid %in% c(sjids.FACHILD, sjids.FACOMS))

# time variable -----------------------------------------------------------
# (removes avisit)

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

dt. %<>% 
  left_join( base ) %>% 
  select ( study, sjid, avisitn, adt, age, time., paramcd, bl, aval )

rm(base)

# changes and intervals ---------------------------------------------------

dt. %<>%
  group_by ( study, sjid, paramcd ) %>% 
  arrange  ( study, sjid, paramcd, avisitn ) %>%
  mutate   ( cbl        = aval - bl ) %>% 
  # mutate   ( window.dev = abs( time. - avisitn ) ) %>% 
  select  ( study, sjid, adt, avisitn, age, time., paramcd, bl, aval, cbl )

# fix visit intervals / adjust windows ------------------------------------

# # figure out deviations
# dt. %>%
#   mutate( avisitn.x    = ifelse(time. < 2.25, round(time.*2)/2, round(time.) ) ) %>%
#   mutate( flag = ifelse(avisitn.x != avisitn, T, F)) %>%
#   mutate( avisitn.save = avisitn ) %>% 
#   mutate( avisitn = avisitn.x ) %>%
#   select( - avisitn.x ) %>% 
#   mutate( window.dev = abs( time. - avisitn ) ) %>%
#   mutate( dev.cat    = cut(window.dev, c(0, 0.25, 0.5, 1, 10), labels = c('<3M', '>3M', '>6M', '>1y'), include.lowest = T ) ) %>%
#   saveRDS ( 'DATA derived/dt.adj.rds' )

# re-assign visit labels / timing ------------------------------------------

dt. %>% 
  group_by(study, sjid, paramcd, avisitn) %>% 
  filter(n()>1)

dt. %<>%
  mutate( avisitn.x    = ifelse(time. < 2.25, round(time.*2)/2, round(time.) ) ) %>%
  # mutate( flag = ifelse(avisitn.x != avisitn, T, F)) %>%
  mutate( avisitn.save = avisitn ) %>%
  mutate( avisitn = avisitn.x ) %>%
  select( - avisitn.x )

dt. %>% 
  filter(study == 'FACOMS') %>% 
  # filter(paramcd %in% c('mFARS','ADL')) %>%
  group_by(study, sjid, paramcd, avisitn) %>%
  filter(n()>1) %>% 
  filter(row_number()==1) %>% 
  summarise(n = n()) %>% 
  ungroup %>% select(-sjid) %>% 
  group_by(study, paramcd, avisitn) %>%
  summarise(N = sum(n)) %>% 
  spread(avisitn, N)

# dt.visits %>%
#   ungroup %>% 
#   filter(sjid %in% sjids.itt) %>% 
#   select( paramcd, avisitn ) %>% 
#   table
# 
# dt.visits %>%
#   ungroup %>% 
#   select(study, sjid, avisitn) %>% unique %>% 
#   filter(avisitn == 0) %>% 
#   ungroup %>% 
#   select( avisitn ) %>% 
#   table

# # follow-up characteristics for BL -----------------------------------------
# # 12 patients that were unable at something at baseline. used as empty
# # first age/dur during a visit is used (sometimes bbs comes from later)
# 
# dt.fu <- dt. %>%
#   group_by ( study, sjid, avisitn ) %>% 
#   mutate_at( vars('age', 'time.'), min ) %>% 
#   select   ( study, sjid, avisitn, age) %>%
#   mutate_at( 'age' , round, 2) %>% 
#   unique %>% 
#   group_by ( study, sjid, age ) %>% 
#   arrange  ( study, sjid, avisitn) %>% 
#   mutate   ( 
#     fu          = max(age)-min(age),
#     visit.count = n(),
#     bl.age      = min(age)
#   ) %>%
#   group_by( study, sjid ) %>% 
#   filter( avisitn == min(avisitn) ) %>%
#   ungroup
# 

# demo --------------------------------------------------------------------

dm. <- .dd('demo') %>% 
  filter( sjid %in% dt.$sjid ) %>% 
  filter( study %in% c('FACHILD', 'FACOMS')) %>% 
  mutate( study = ifelse(sjid %in% sjids.FACHILD, 'FACHILD', NA)) %>% 
  mutate( study = ifelse(sjid %in% sjids.FACOMS , 'FACOMS' , study)) %>% 
  select( study, site, sjid, sex, aoo, gaa1, pm, birthdt ) %>% 
  filter( !is.na( study ))

# add steps ---------------------------------------------------------------
# only baseline!

dm. %<>% 
  left_join( 
    .dd.atx('steps', c = T) %>% 
      filter(avisitn == 0) %>% 
      select(study, sjid, bl.amb = amb)
    ) %>% 
  filter(!is.na(bl.amb))

# dm. <- dt.fu %>% 
#   left_join( dm. %>% select(-birthdt) ) %>% 
#   # select   ( -bl.age ) %>% 
#   select   ( study, site, sjid, avisitn, sex, symp, gaa1, pm, bl.age, fu, visit.count, everything() ) 
# 
# # names(dm.)
# rm(dt.fu)

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

# add subgroups, based on all itt  ----------------------------------------

subgroup.age <- dt. %>% ungroup %>%
  filter( 
    paramcd == 'mFARS', study == 'FACHILD', sjid %in% sjids.itt, avisitn == 0
    ) %>% 
  left_join(dm. %>% select(study, sjid, bl.amb)) %>% filter( bl.amb == 'ambulatory' ) %>% 
  group_by( study ) %>% 
  mutate( median = median (age) ) %>% 
  mutate( median = ifelse(study == 'FACOMS', NA, median)) %>% 
  ungroup( ) %>% 
  mutate( median = mean(median, na.rm=T) ) %>% 
  mutate( subgroup.age = ifelse(aval >= median , paste0('>', round(median ,1)), paste0('>', round(median ,1)))) %>%
  select(sjid, subgroup.age) %>% 
  unique

# median mFARS subgroups --------------------------------------------------

subgroup.mFARS <- dt. %>% ungroup %>%
  filter( 
    paramcd == 'mFARS', study == 'FACHILD', sjid %in% sjids.itt, avisitn == 0
  ) %>% 
  left_join(dm. %>% select(study, sjid, bl.amb)) %>% filter( bl.amb == 'ambulatory' ) %>% 
  group_by( study ) %>% 
  mutate( median = median (aval) ) %>% 
  mutate( median = ifelse(study == 'FACOMS', NA, median)) %>% 
  ungroup( ) %>% 
  mutate( median = mean(median, na.rm=T) ) %>% 
  mutate( subgroup.mFARS = ifelse(aval >= median , paste0('>', round(median ,1)), paste0('>', round(median ,1)))) %>%
  select(sjid, subgroup.mFARS) %>% 
  unique

# median FARS.E subgroups -------------------------------------------------

subgroup.FARS.E <- dt. %>% ungroup %>%
  filter( 
    paramcd == 'FARS.E', sjid %in% sjids.itt, avisitn == 0
  ) %>% 
  left_join(dm. %>% select(study, sjid, bl.amb)) %>% filter( bl.amb == 'ambulatory' ) %>% 
  group_by( study ) %>% 
  mutate( median = median (aval) ) %>% 
  mutate( median = ifelse(study == 'FACOMS', NA, median)) %>% 
  ungroup( ) %>% 
  mutate( median = mean(median, na.rm=T) ) %>% 
  mutate( subgroup.FARS.E = ifelse(aval >= median , paste0('>', round(median ,1)), paste0('>', round(median ,1)))) %>%
  select(sjid, subgroup.FARS.E) %>% 
  unique

dm. %<>% 
  left_join( subgroup.age ) %>%
  left_join( subgroup.FARS.E ) %>%
  left_join( subgroup.mFARS  ) %>% 
  mutate( itt = ifelse(sjid %in% sjids.itt, T, F))

# write -------------------------------------------------------------------

dm. %>% 
  saveRDS ( 'DATA derived/dm.rds' )

dt. %>% 
  saveRDS ( 'DATA derived/dt.rds' )

rm(pars., params., subgroup.age, subgroup.FARS.E, subgroup.mFARS)
rm(exclude.mFARS.17, exclude.mFARS.n1, sjids.FACHILD, sjids.FACOMS, sjids.itt)


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
