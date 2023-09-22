
# read data ---------------------------------------------------------------
rm(list=ls())

# source('list.pars.R')
require(labelled)

dt.          <- readRDS('DATA derived/dt.rds')
# dt.long.unable <- readRDS('DATA derived/dt.long.rds') %>% 
#   ungroup %>% select(sjid, avisitn, paramcd, unable) %>% filter(!is.na(unable))

options(digits = 5)

# follow up type barchart ----------------------------

dt. <- bind_rows(
  dt.bl %>% mutate(group = 'all')      %>% select(-med.age, -med.FARS.E, amb),
  dt.bl %>% mutate(group = amb)        %>% select(-med.age, -med.FARS.E, amb),
  dt.bl %>% mutate(group = med.age)    %>% select(-med.age, -med.FARS.E, amb),
  dt.bl %>% mutate(group = med.FARS.E) %>% select(-med.age, -med.FARS.E, amb)
) %>% 
  filter(!is.na(group)) %>%
  gather(paramcd, aval, pars.) %>% 
  left_join(dt.long.unable) %>% 
  mutate(unable = ifelse(is.na(unable), F, unable)) %>% 
  mutate_at('paramcd', factor, pars. ) %>% 
  mutate_at('group', factor, c('all','ambulatory','non-amb.', 'age < 13.9', 'age > 13.9', 'FARS.E < 22.8', 'FARS.E > 22.8') ) %>%
  group_by(group) %>% 
  select(sjid, avisitn, paramcd, aval, unable) %>% 
  group_by(paramcd, .add = T)

# mutate(result = paste(sprintf('%.2g', m), ' (', sprintf('%.2g', sd), ')', sep = '' )) %>% 

dt <- dt. %>% 
  filter(!grepl('.iu', paramcd)) %>% 
  select(-avisitn) %>% 
  # filter(paramcd == 'mFARS') %>% 
  # filter(group   == 'all') %>% 
  group_by(paramcd, group) %>% 
  mutate(N = n()) %>% 
  filter(!is.na(aval)) %>%
  group_by(paramcd, group, N) %>% 
  filter( unable == F ) %>% 
  summarise(n = n()) %>% 
  mutate(pct = round(100*n/N,0))

ft1 <- dt %>% 
  select(-n) %>% 
  # filter(paramcd %in% c(pars.[c(1,2,3,4,5)])) %>% 
  spread(paramcd, pct) %>% 
  flextable() %>% 
  width(width = 12.8/14) %>% 
  fontsize(size = 16, part = "all") %>% 
  height_all(5.47/8, part = "all") 

dt <- dt. %>% 
  filter(!grepl('.iu', paramcd)) %>% 
  select(-avisitn) %>% 
  # filter(paramcd == 'mFARS') %>% 
  # filter(group   == 'all') %>% 
  group_by(paramcd, group) %>% 
  mutate(N = n()) %>% 
  filter(!is.na(aval)) %>%
  group_by(paramcd, group, N) %>% 
  filter( unable == F ) %>% 
  summarise(n = n(), m = mean(aval), s = sd(aval)) %>%
  mutate( pct = round(100*n/N,0) ) %>% 
  mutate(res = ifelse(paramcd %in% c('tug.i'), 
                      paste(sprintf('%.3f', m), '\n(', sprintf('%.3f', s), ')', sep = ''),
                      paste(sprintf('%.2f', m), '\n(', sprintf('%.2f', s), ')', sep = '')
                      ))


ft2 <- dt %>% 
  select(-n, -m, -pct, -s) %>% 
  # filter(paramcd %in% c(pars.[c(1,2,3,4,5)])) %>% 
  spread(paramcd, res) %>% 
  flextable() %>% 
  width(width = 12.8/14) %>% 
  fontsize(size = 16, part = "all") %>% 
  height_all(5.47/8, part = "all")

read_pptx( '../Templates/CR.template.pptx' ) %>%
  add_slide   ( layout = '1', master = 'CR') %>%
  ph_with     ( ft1, location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide   ( layout = '1', master = 'CR') %>%
  ph_with     ( ft2, location = ph_location_type( type = "body" , id = 1) ) %>%
  print ( target = paste('Baseline.Data.Tables.', gsub(":","-", Sys.time()), ".pptx", sep="") )
