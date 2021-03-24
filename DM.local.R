
# setup -------------------------------------------------------------------

rm(list=ls())

require('labelled')
source('list.pars.R')

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

# factor labels -----------------------------------------------------------

dt. %<>% 
  mutate(paramcd = factor(paramcd, pars.)) %>% 
  mutate(param   = factor(paramcd, pars., labels = params.))

demo. <- .dd.FA('demo') %>% 
  filter( sjid %in% dt.$sjid ) %>% 
  mutate(study = 'FACHILD')

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
  left_join( steps )

dt. %<>% 
  group_by(sjid, amb) %>%
  mutate( time. = as.numeric( adt - min(adt) ) / 365.25) %>%
  .add.time( tm = 'age', keepadt = T) %>% 
  # filter ( is.na(amb) ) %>% ### !!!
  group_by(sjid)

rm(steps)

# remove non-ambulatory t25fw ---------------------------------------------

# need to be kept
# dt. %>% 
#   filter ((paramcd == 'w25.i' & amb == 'non-amb.')) %>%
#   filter (!(paramcd == 'w25.iu' & amb == 'non-amb.')) 

# save baseline by amb ----------------------------------------------------

base <- dt. %>%
  filter  ( !is.na(amb)) %>% # should affect no pne at BL
  group_by( study, sjid, paramcd, amb) %>% # baseline by-amb!!
  filter  ( avisitn == min ( avisitn ) ) %>%
  group_by(sjid, paramcd, amb) %>% 
  arrange(sjid, paramcd, amb) %>% 
  rename  ( bl.age = age, bl = aval ) %>%
  ungroup %>% 
  select  ( study, sjid, avisitn, amb, paramcd, bl, bl.age)

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

dt.long <- dt.

dt.long %<>% 
  # mutate(hpf = ifelse(is.na(hpf) | hpf == 'In-Person', 'In-Person', hpf)) %>%
  mutate(hpf = factor(hpf, c('Video', 'Audio Only', 'In-Person'))) 

rm(dt.)
# . -----------------------------------------------------------------------
# 12 patients that were unable at something at baseline. used as empty
# first age/dur during a visit is used (sometimes bbs comes from later)

dt.bl <- dt.long %>% 
  select(-fpf) %>% 
  # filter( !(paramcd %in% c('w25.iu', 'hpt.iu')) ) %>% 
  # select( sjid, age, sex, avisitn, symp, gaa1, pm, step, med.age, fds, amb, paramcd, aval) %>%
  group_by ( sjid ) %>% 
  arrange(sjid, paramcd, avisitn) %>% 
  mutate( 
    fu       = max(age)-min(age),
    fu_v     = max(avisitn)-1,
    bl.age   = min(age)
  ) %>%
  select(-c(param, time., age, adt, unable, dev.y, hpf, cbl)) %>% 
  # filter(!(paramcd %in% c('FARS.Am', 'FARS.C'))) %>% 
  spread( paramcd , aval ) %>% 
  group_by( sjid ) %>% 
  filter( avisitn == min(avisitn) ) %>% 
  ungroup

dt.bl %>% group_by(sjid) %>% filter(n()>1) %>% arrange()

# add subgroups and follow up stats ---------------------------------------

med.bl.age    <- median(dt.bl$bl.age)  
med.bl.FARS.E <- median(dt.bl$FARS.E, na.rm=T)  

dt.bl %<>% 
  mutate(med.age    = case_when(
    bl.age > med.bl.age ~ paste('age >', round(med.bl.age,1)),
    bl.age < med.bl.age ~ paste('age <', round(med.bl.age,1))
    )) %>% 
  mutate(med.FARS.E = case_when(
    FARS.E > med.bl.FARS.E ~ paste('FARS.E >', round(med.bl.FARS.E,1)),
    FARS.E < med.bl.FARS.E ~ paste('FARS.E <', round(med.bl.FARS.E,1)))
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




dt.bl %>% 
  saveRDS ( 'DATA derived/dt.bl.rds' )

dt.long %>% 
  saveRDS ( 'DATA derived/dt.long.rds' )
