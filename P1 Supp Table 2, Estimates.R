
# setup ---------------------------------------------------------------

rm(list=ls())
source('project.settings.R')

# data --------------------------------------------------------------------

parids  <- c(1,2,3)  # "mFARS"  "FARS.E" "FARS.B" "FARS.C"
# parids <- c(6,8,9,10) # "w25.i"  "tug.i"   "w1m"     "w6m"
# parids <- c(13,14,15,16) # PedsQL Total and Physical

pars.   <- pars.[c(parids)] 

est.aval <- readRDS('DATA derived/all estimates.rds') %>% 
  mutate ( param = as.character(param)) %>% 
  mutate ( param  = ifelse ( param == 'mFARS', 'mFARS Total', param )) %>%
  mutate ( param  = factor(param, levels = params.)) %>% 
  filter ( paramcd %in% pars.) %>% 
  # filter ( pop == 'all' ) %>% 
  droplevels()

supp.table <- est.aval %>% ungroup %>% 
  filter( pop == 'all' ) %>%
  filter( paramcd != 'FARS.C' ) %>%
  filter( avisit != 'BL' ) %>% 
  mutate( pop = ifelse(pop == 'all', pop, paste(pop, value, sep = '')) ) %>% 
  mutate_at(vars('estimate','conf.low','conf.high'), round, 1) %>% 
  mutate_at(vars('estimate','conf.low','conf.high'), ~sprintf("%.1f", .)) %>% 
  mutate( CI = paste( '(', conf.low, ', ', conf.high, ')', sep = '' )) %>% 
  mutate(significance = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    p.value < 0.1   ~ ".",
    TRUE            ~ ""
  )) %>% 
  mutate(p.value = ifelse(p.value < 0.0001, '<0.0001', sprintf('%.4f', p.value))) %>% 
  select(study, pop, paramcd, avisit, estimate, CI, p.value, significance) %>% 
  arrange(study, paramcd)

supp.table %>% 
  left_join ( readRDS ('DATA derived/SRMs.rds') %>% ungroup %>%  select(study, pop, paramcd, avisit, n, m, s, SRM)) %>% 
  select    ( -pop, -s, -m ) %>% 
  filter    ( !(avisit %in% c('6m','18m')) ) %>% 
  mutate    ( SRM = sprintf('%.2f', SRM)) %>% 
  .ct
  as.data.frame()
