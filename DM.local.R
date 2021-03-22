
rm(list=ls())

pars.      <- c('tug.i', 'w1m', 'w6m', 'w25.i', 'hpt.i','bbs','ADL','mFARS','FARS.E','FARS.B','FARS.C','FARS.Am')
par.labels <- c('Timed up and Go (1/s)', 
                'One Minute Walk (m)', 
                '6 Minute Walk (m)', 
                'Timed 25 Foot Walk (m/s)', 
                '9 Hole Peg Test (1/min)',
                'Berg Balance Scale',
                'Activities of Daily Living',
                'mFARS',
                'Upright Stability (FARS.E)',
                'Upper Limbs (FARS.B)',
                'Lower Limbs (FARS.C)',
                'Bulbar (FARS.Am)')

dt.cat <- .dd.FA('scafi_cat', c = T) %>% 
  filter(study == 'FACHILD')

dt. <- bind_rows(
  .dd.FA('fars', c = T),
  .dd.FA('scafi', c = T) %>% filter(!(sjid == 5136 & paramcd == 'tug.i' & aval == 2)), # tug in two seconds
  .dd.FA('bbs', c = T),
  .dd.FA('adl', c = T)
  ) %>% 
  filter(paramcd %in% pars.) %>% 
  filter( study == 'FACHILD' ) %>%
  left_join( .dd.FA('steps', c = T) %>% filter(study == 'FACHILD') %>% select(study, sjid, avisitn, step, fds, amb)) %>%
  .add.time(tm = 'age') %>% 
  .add.demo() %>%
  filter(!is.na(age)) %>% 
  mutate(param = factor(paramcd, pars., labels = par.labels))

demo. <- .dd.FA('demo') %>% 
  filter( sjid %in% dt.$sjid )

# fill scales NA with F

dt. %<>% 
  mutate(unable = ifelse(paramcd %in% pars.[c(6:12)], F, unable))


with(dt., table(paramcd, unable), exclude = F)

# add BL and by visit FARS E and E7 ---------------------------------------

tmp <- .dd.FA('fars', c = T) %>% 
  filter( study == 'FACHILD' ) %>% 
  filter( paramcd %in% c('FARS.E', 'fane7')) %>% 
  left_join( .dd.FA('steps', c = T) %>% filter(study == 'FACHILD') %>% select(study, sjid, avisitn, step, fds, amb)) %>%
  .add.time(tm = 'age', keepadt = T) %>% 
  .add.demo() %>%
  select( study, sjid, avisitn, adt, age, symp, gaa1, pm, paramcd, aval) %>% 
  spread( paramcd, aval )

tmp1 <- tmp  %>% 
  filter( avisitn == 1) %>% 
  rename( fane7_bl = fane7, 
          FARS.E_bl = FARS.E) %>%
  rename( age_bl = age ) %>% 
  select( sjid, contains('_bl'))

tmp %<>% 
  left_join(tmp1)

dt. %<>% 
  left_join(tmp)

# baseline and changes ----------------------------------------------------

# save aval and empty unables
dt. %<>%
  rename ( sv.aval = aval ) %>%
  mutate ( aval    = ifelse(unable == F, sv.aval, NA) )

bl <- dt. %>% 
  group_by( paramcd, sjid ) %>% 
  filter(!is.na(aval)) %>% 
  filter(avisitn == 1) %>% rename(bl = aval) %>% 
  select(sjid, paramcd, bl)

dt. %<>% 
  left_join(bl)

# changes -----------------------------------------------------------------

dt. %<>% 
  # filter( paramcd == 'w6m', sjid == 4857) %>%
  group_by( sjid, paramcd) %>% 
  arrange( sjid, paramcd, adt) %>% 
  mutate( int = as.numeric(lead(adt)-adt)/365.25) %>% 
  mutate( chg = ( lead(aval)-aval)) %>% 
  mutate( chg = ifelse(lead(avisitn)-avisitn > 1, NA, chg)) %>% 
  mutate( cbl = aval-bl) 

filter(dt., sjid %in% c(4657, 4752, 4946), paramcd == 'w1m')

with(dt. %>%  filter(!is.na(cbl)), table(paramcd, avisitn, unable))

# dt1. <- dt. %>% 
#   filter(avisitn %in% c(1,3,5)) %>% 
#   # filter( paramcd == 'w6m', sjid == 4857) %>% 
#   group_by( sjid, paramcd) %>% 
#   arrange( sjid, paramcd, adt) %>% 
#   mutate( int = as.numeric(lead(adt)-adt)/365.25) %>% 
#   mutate( chg = ( lead(aval)-aval)) %>% 
#   mutate( chg = ifelse(lead(avisitn)-avisitn != 2, NA, chg)) %>% 
#   mutate( unable = ifelse(paramcd %in% pars.[c(1,2,3,4,5)], unable, F)) %>% 
#   mutate( chg = ifelse((lead(unable) == T), NA, chg)) %>% 
#   mutate( chg = ifelse(      unable  == T , NA, chg))

# age-group ---------------------------------------------------------------

tmp <- dt. %>% group_by(sjid) %>% select(sjid, avisitn, age) %>% filter( avisitn == 1 ) %>% unique

med.bl.age <- median(tmp$age)  

age.groups <- tmp %>% mutate(age.groups = ifelse(age<med.bl.age, '<13.8y', '>13.8y')) %>% select(-age)

rm(tmp)

dt. %<>%
  mutate ( age.groups = ifelse ( age < med.bl.age, '<13.8y', '>13.8y'))


# sort --------------------------------------------------------------------

# dt. %<>% 
#   select(study, sjid, age_bl, age.groups, age_bl, fane7_bl, FARS.E_bl, symp, gaa1, pm, adt, avisitn, age, dur, step, fds, amb, fane7, FARS.E, paramcd, param, bl, aval, cbl, chg, unable)

# dt1. %<>% 
#   select(study, sjid, age_bl, age.groups, symp, gaa1, pm, adt, avisitn, age, dur, step, fds, amb, fane7, FARS.E, paramcd, param, bl, aval, cbl, chg, unable)

# write -------------------------------------------------------------------


dt. %>% 
  write_rds('DATA derived/dt.rds')

dt. %>% 
  filter(sjid == 4186, paramcd == 'ADL') %>%
  filter(!(sjid == 4228 & paramcd == 'bbs')) %>%
  group_by(sjid, paramcd) %>% 
  mutate(obs. = length(unique(avisitn))) %>% #select(avisitn, paramcd) %>% table
  mutate(max = max(cbl, na.rm=T)) %>% 
  mutate(min = min(cbl, na.rm=T)) %>% 
  mutate(mchg = ifelse(paramcd %in% c('bbs', 'hpt.i', 'w25.i', 'tug.i', 'w1m', 'w6m'), min, max)) %>% 
  .wt('.DATA derived/dt.txt')

