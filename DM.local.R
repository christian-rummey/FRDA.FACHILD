# setup -------------------------------------------------------------------

rm(list=ls())
source('project.settings.R')

# pars. <- pars.[c(1,2,3,4)]#, .l.FARS.E[c(2,3,4,5,6,7,9)])
# pars. <- pars.[c(1,2,12)]#, .l.FARS.E[c(2,3,4,5,6,7,9)])

# DATA --------------------------------------------------------------------

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
  filter(!is.na(aval)) %>% 
  select(study, sjid, avisit, avisitn, adt, age, paramcd, aval)

sjids.FACHILD <- .rd.FACHILD('demo') %>% 
  select(sjid) %>% deframe

dt. %<>% group_by( study, sjid, paramcd ) %>% 
  mutate(time. = age-min(age))

dt. <- bind_rows(
    dt. %>% filter(study == 'FACHILD'),
    dt. %>% filter(study == 'FACOMS' ) %>% filter(!(sjid %in% sjids.FACHILD))
    ) %>% 
  select(-avisit)

# reduce FACOMS to relevant age range -------------------------------------
# FACHILD maximum BL age is 17.9, remove non-eligible FACOMS patients

dt. %>% ungroup %>% 
  filter(study == 'FACHILD') %>% select( age ) %>% 
  range()

dt. %<>% 
  filter(age > 6.6  ) %>%
  filter(age < 21.2 )

dt. %<>% 
  group_by( study, sjid ) %>% 
  filter  ( min( age ) < 18 ) %>% 
  ungroup

# reduce visits to mFARS available (FACOMS!) ------------------------------
# FACHILD needs to stay due to virtual visits

dt.visits <- dt. %>% 
  select(-age, -time., -adt) %>% 
  spread(paramcd, aval) %>% 
  # select(study, sjid, avisitn, mFARS, bbs, ADL, TOT.P, TOT.C, hpt.i, w25.i, w1m, tug.i, w6m) %>% .ct
  group_by(sjid, avisitn) %>%
  # filter(n()>1) %>% arrange(sjid) %>%
  filter( (!is.na(mFARS) | study == 'FACHILD') ) %>% 
  select( study, sjid, avisitn )

dt. %<>% 
  right_join(dt.visits) %>% 
  group_by(sjid) %>% 
  mutate(avisitn = avisitn - min(avisitn))
  # filter(avisitnx != avisitn) %>% ungroup %>% select(study) %>% table
  # filter(min(avisitn)!=0)

dt. %<>% 
  mutate( paramcd = factor(paramcd, pars.))

rm(dt.visits)

# time. to 0; avisitn only used for FACHILD visit stats; not need fix -------

dt. %>% 
  group_by(study) %>% 
  select(study, sjid, avisitn) %>% unique %>% 
  select(study, avisitn) %>% table

dt. %<>%
  group_by(study, sjid ) %>%
  mutate( time. = age-min(age)) %>%
  select( study, sjid, avisitn, adt, age, time., paramcd, aval )

dt. %<>% 
  filter( !( (study == 'FACOMS') & time.>3.5 ) ) %>% 
  ungroup

# add steps; only necessary for BL -------------------------------------------

bl.amb.status <- dt. %>%
  left_join( 
    .dd.atx( 'steps', c = T ) %>%
      select( study, sjid, avisitn, amb, e7.act )
  ) %>% 
  select (study, sjid, time., avisitn, amb, e7.act) %>%
  group_by( study, sjid) %>%
  filter  ( avisitn == min(avisitn) ) %>% unique %>%  
  mutate ( bl.amb = amb ) %>%
  mutate ( bl.amb = ifelse( amb == 'ambulatory' | e7.act < 5, 'ambulatory', bl.amb) ) %>%
  filter ( !is.na(bl.amb)) %>% 
  select ( study, sjid, bl.amb )

# AEN/ITT Definitions ---------------------------------------------------------
# AEN includes all FACHILD pts, but only restricted FACOMS pop
# pts from FACHILD that miss any criteria will be in data sets, but not in ITT
# 1) fit age range, 2) BL mFARS >= 17, 3) n(fars)>1 4) FDS OR E7<5 

sjids.aen <- dt. %>% filter(sjid %in% bl.amb.status$sjid) %>% select(sjid) %>% unique() %>% deframe
  
#  < 2 mFARS visitS 
exclude.mFARS.n1  <- dt. %>% filter(paramcd == 'mFARS' ) %>% group_by(study, sjid) %>% filter(n()==1) %>% ungroup %>% select(sjid) %>% deframe
# mFARS BL < 17 
exclude.mFARS.17  <- dt. %>% filter(paramcd == 'mFARS' & avisitn == 0 & aval < 17) %>% ungroup %>% select(sjid) %>% deframe
# bl.non.amb
exclude.bl.nonamb <- bl.amb.status %>% filter(bl.amb == 'non-amb.') %>% ungroup %>% select(sjid) %>% deframe

# FACOMS with < 1 visit # this also removes 4851 (no bl) from itt, and 4787, 4866, 4882; 
# these are 17-20 and stay in: 4916, 4928, 4934, 4942, 4998
# see "FACHILD Baseline Criteria.R"

sjids.itt <- sjids.aen

sjids.itt <- discard(sjids.itt, ~.x %in% c(exclude.mFARS.n1, exclude.mFARS.17, exclude.bl.nonamb)) # missing FACHILD BL visit
sjids.itt <- discard(sjids.itt, ~.x == 4851) # missing FACHILD BL visit
sjids.itt <- discard(sjids.itt, ~.x ==   80) # first fars visit != BL (FACOMS)

# data sets will have "all enrolled" for FACHILD, but only pts with 2 FARS FU for FACOMS --------

dt. %<>% 
  filter( (sjid %in% sjids.FACHILD | sjid %in% sjids.itt ) )

dt. %>% group_by(study) %>% summarise(length(unique(sjid)))

sjids.FACOMS <- dt. %>% 
  filter( study == 'FACOMS' & paramcd == 'mFARS') %>%
  group_by( sjid ) %>% 
  filter( !is.na(aval) ) %>% 
  filter( n()>1 ) %>%
  # filter( min(avisitn) == 0 ) %>% # that removes 8 with first visit not 0
  select( sjid ) %>% deframe() %>% unique()

dt. %>% 
  filter( study == 'FACHILD'  | sjid %in% sjids.FACOMS ) %>% 
  group_by(sjid)

# POPULATIONS DONE --------------------------------------------------------

# fix visit intervals / adjust windows ------------------------------------
# figure out deviations

# save previous intervals for FACHILD visit stats
dt. %>% 
  filter(!is.na(aval)) %>% 
  # filter(study != 'FACHILD') %>%
  # filter(paramcd != 'mFARS') %>% 
  mutate(par = ifelse(paramcd == 'mFARS','FARS','ALL')) %>%
  ungroup %>% 
  select(study, avisitn, sjid, par) %>% 
  unique %>% 
  .wds('../FRDA.FACHILD/DATA derived/visit.stats')

# re-assign visit labels / timing ------------------------------------------

# there are not double visits before rounding
dt. %>% 
  group_by(study, sjid, paramcd, avisitn) %>% 
  filter(n()>1)

# round FACHILD avisitn to half numbers

dt. %<>%
  mutate ( avisitn.x    = ifelse(study == 'FACHILD' & time. < 2.25, round(time.*2)/2, round(time.) ) ) %>%
  mutate ( avisitn.save = avisitn ) %>%
  mutate ( avisitn = avisitn.x ) %>%
  select ( -avisitn.x, -avisitn.save )

# 18 mFARS visits get averaged (6 in FACHILD) ---------------------------------

dt. %>% 
  filter(paramcd == 'mFARS') %>%
  group_by(study, sjid, avisitn, paramcd) %>% 
  filter(n()>1) %>% 
  group_by(study, sjid, avisitn) %>% 
  select(study, sjid, avisitn) %>% 
  unique %>% ungroup %>% 
  select(study) %>% table

dt. %<>% 
  group_by(study, sjid, paramcd, avisitn) %>% 
  mutate_at(vars(adt, age, time., aval), mean) %>%
  select(-adt) %>% 
  unique()

# follow-up data (after adjustment) ---------------------------------------

fu.data <- dt. %>%
  filter   ( paramcd == 'mFARS' ) %>%
  group_by ( study, sjid, avisitn ) %>%
  mutate_at( vars('age', 'time.'), min ) %>%
  select   ( study, sjid, avisitn, age) %>%
  mutate_at( 'age' , round, 2) %>%
  unique %>%
  group_by ( study, sjid, age ) %>%
  arrange  ( study, sjid, avisitn) %>%
  group_by ( study, sjid) %>%
  mutate   (
    fu          = max(age)-min(age),
    visit.count = n(),
    bl.data.age = min(age)
  ) %>%
  group_by( study, sjid ) %>%
  filter( avisitn == min(avisitn) ) %>%
  ungroup

# only checking here; has been done above ----------------------------------
# there are 51 FACHILD visits w/o mFARS
# none in FACOMS

dt. %>% 
  select(-age, -time.) %>% 
  spread(paramcd, aval) %>%
  filter(is.na(mFARS)) #%>% print(n=51)

# Baseline Values, changes and intervals --------------------------------------
# moved down after interval adjustment 22/09/23

base <- dt. %>%
  ungroup %>% 
  filter  ( avisitn == 0) %>% 
  select  ( study, sjid, paramcd, bl = aval)

dt. %<>% 
  left_join( base ) %>% 
  mutate   ( cbl        = aval - bl ) %>% 
  select   ( study, sjid, avisitn, paramcd, bl, aval, cbl ) %>% 
  arrange  ( study, sjid, avisitn, paramcd )

rm(base)

# demo --------------------------------------------------------------------

dm. <- .dd('demo.l') %>% 
  filter( sjid %in% unique(dt.$sjid) ) %>% 
  mutate( study = ifelse(sjid %in% sjids.FACHILD, 'FACHILD', NA)) %>% 
  mutate( study = ifelse(sjid %in% sjids.FACOMS , 'FACOMS' , study)) %>% 
  select( study, site, sjid, sex, aoo, gaa1, gaa2, pm, birthdt, bl.std.demo.age = age_bl ) %>% 
  filter( study %in% c('FACHILD', 'FACOMS')) %>% 
  filter( !is.na( study) )

# what comes from DEMO is rfstdt in FACOMS. Need to use bl.date from dt.data
dm. %>% 
  left_join(fu.data) %>% 
  mutate(x = bl.std.demo.age-bl.data.age) %>% 
  arrange(x) %>%
  print(n=10)

dm. %<>% 
  left_join( fu.data ) %>% 
  mutate( bl.age = bl.data.age) %>% 
  select( study, site, sjid, sex, aoo, gaa1, gaa2, pm, bl.age, fu, visit.count) %>% 
  mutate( itt    = ifelse(sjid %in% sjids.itt        , 'Y','N')) %>% 
  mutate( bl.amb = ifelse(sjid %in% exclude.bl.nonamb, 'non-amb.','ambulatory'))

with(dm., table(study, bl.amb))
with(dm. %>% filter(study == 'FACHILD'), table(itt, bl.amb))

dt. %<>% 
  filter(sjid %in% unique(dm.$sjid))

# factor labels -----------------------------------------------------------

dt. %<>%
  mutate(paramcd = factor(paramcd, pars.)) %>%
  droplevels()

# add subgroups, based on ambulatoy itt -----------------------------------

subgroup.age <- dt. %>% ungroup %>%
  filter( 
    paramcd == 'mFARS', sjid %in% sjids.itt, avisitn == 0
    ) %>% 
  left_join(dm. %>% select(study, sjid, bl.amb, bl.age)) %>% filter( bl.amb == 'ambulatory' ) %>% 
  group_by( study ) %>% 
  mutate( median. = median (bl.age) ) %>% 
  mutate( median. = ifelse(study == 'FACOMS', NA, median.)) %>% 
  ungroup( ) %>% 
  mutate( median. = mean(median., na.rm=T) ) %>% 
  mutate( subgroup.age = ifelse( bl.age < median. , paste0('<', round(median. ,1)), paste0('>', round(median. ,1)))) %>%
  select( study, sjid, subgroup.age) %>% 
  unique

# median mFARS subgroups --------------------------------------------------

subgroup.mFARS <- dt. %>% ungroup %>%
  filter( 
    paramcd == 'mFARS', sjid %in% sjids.itt, avisitn == 0
  ) %>% 
  left_join(dm. %>% select(study, sjid, bl.amb)) %>% filter( bl.amb == 'ambulatory' ) %>% 
  group_by( study ) %>% 
  mutate( median. = median (aval) ) %>% 
  mutate( median. = ifelse(study == 'FACOMS', NA, median.)) %>% 
  ungroup( ) %>% 
  mutate( median. = mean(median., na.rm=T) ) %>% 
  mutate( subgroup.mFARS = ifelse(aval <= median. , paste0('<=', round(median. ,1)), paste0('>', round(median. ,1)))) %>%
  select( study, sjid, subgroup.mFARS) %>% 
  unique

# median FARS.E subgroups -------------------------------------------------

subgroup.FARS.E <- dt. %>% ungroup %>%
  filter( 
    paramcd == 'FARS.E', sjid %in% sjids.itt, avisitn == 0
  ) %>% 
  left_join(dm. %>% select(study, sjid, bl.amb)) %>% filter( bl.amb == 'ambulatory' ) %>% 
  # arrange(aval)
  group_by( study ) %>% 
  mutate( median. = median (aval) ) %>% 
  mutate( median. = ifelse(study == 'FACOMS', NA, median.)) %>% 
  ungroup( ) %>% 
  mutate( median. = mean(median., na.rm=T) ) %>% 
  mutate( subgroup.FARS.E = ifelse(aval >= median. , paste0('<', round(median. ,2)), paste0('>=', round(median. ,2)))) %>%
  select( study, sjid, subgroup.FARS.E) %>% 
  unique

subgroup.age    %>% select(study, subgroup.age    ) %>% table
subgroup.FARS.E %>% select(study, subgroup.FARS.E ) %>% table
subgroup.mFARS  %>% select(study, subgroup.mFARS  ) %>% table

dm. %<>% 
  left_join( subgroup.age ) %>%
  left_join( subgroup.FARS.E ) %>%
  left_join( subgroup.mFARS  ) %>% 
  mutate( itt = ifelse(sjid %in% sjids.itt, T, F))

# write -------------------------------------------------------------------

dm. %>% 
  saveRDS ( 'DATA derived/dm.rds' )

dt. %<>% 
  mutate( avisit = factor(avisitn, c(0,0.5,1,1.5,2,3)) ) %>% 
  mutate( avisit = fct_recode(avisit,
                              "BL" = "0",
                              "6m" = "0.5",
                              "1y" = "1",
                              "18m" = "1.5",
                              "2y" = "2",
                              "3y" = "3"))

dt. %>% 
  filter  ( !is.na(cbl) ) %>% 
  saveRDS ( 'DATA derived/dt.rds' )

rm(pars., params., subgroup.age, subgroup.FARS.E, subgroup.mFARS)
rm(exclude.mFARS.17, exclude.mFARS.n1, exclude.bl.nonamb, sjids.FACHILD, sjids.FACOMS, sjids.itt, sjids.aen)

# populations -------------------------------------------------------------

dt. %>% 
  filter(avisitn == 0, cbl != 0)

dt. %>% 
  filter( paramcd == 'mFARS') %>% 
  group_by(sjid) %>% 
  filter(n()==1)

dt. %>% 
  filter( paramcd == 'mFARS', avisitn == 0) %>% 
  filter( aval < 20)

dt. %>% 
  filter(paramcd %in% c('mFARs', 'FARS.E', 'FARS.B', 'FARS.C')) %>% 
  select(-cbl, -bl) %>% 
  spread(paramcd, aval) %>% 
  left_join(dm. %>% select(sjid, pm)) %>% 
  filter(pm %in%  c('G130V','I154F')) %>% 
  print(n=23)
