# setup -------------------------------------------------------------------

rm(list=ls())
source('project.settings.R')
options(digits = 5)

# data --------------------------------------------------------------------

dm. <- readRDS('DATA derived/dm.rds')

dt. <- readRDS('DATA derived/dt.rds') %>% 
  filter( avisitn == 0 ) %>% 
  filter( paramcd %in% c('mFARS','FARS.E','FARS.B','FARS.C','FARS.Am') ) %>% 
  select( -aval, -cbl, -avisitn) %>% 
  # mutate_at(vars('age'), round, 2) %>%
  spread( paramcd, bl )

dm. %<>% 
  left_join(dt.) %>% 
  select(-starts_with('subgroup'))

rm(dt.)

# switches for output -----------------------------------------------------

dm. %<>% 
  mutate( pm.yn = ifelse(pm == 0, F, T))

dm. %<>% 
  droplevels

var_label(dm.) <- c("study", "site", "sjid", "sex", "Age of Onset", "GAA1", "GAA2", "pm",
                    "Age",'Fullow Up [y]','visit.count', 
                          "itt", "Ambulation Status", "mFARS Total", 
                          "Upright Stability (E)", "Upper Limbs (B)", "Lower Limbs (C)", "Bulbar Function (A)", 'Compound Heterozygotes')

# Demo Table --------------------------------------------------------------

tb <- tableone::CreateTableOne(
    # vars       = c('sex','aoo', 'gaa1', 'pm', 'bl.age', 'vc', 'fu',  'amb', 'mFARS', 'mFARS20', 'FARS.E','ADL'),
    vars       = c('sex', 'bl.age','aoo', 'gaa1', 'gaa2', 'pm.yn', 'mFARS', 'FARS.E','FARS.B','FARS.C','FARS.Am'),
    factorVars = c('sex','bl.amb', 'pm.yn'),
    strata     = 'study',
    data       = dm. %>% filter(itt)
  ) %>% 
    print(
      varLabels = T,
      nonnormal = c('aoo','gaa1','gaa2'),
      contDigits = 1, catDigits = 1,
      missing = F,
      test = F,
      explain = F, dropEqual= F,
      add.rownames = T, noSpaces = T) %>%
    data.frame() %>% rownames_to_column() %>%
    flextable() %>% 
    fontsize(size = 14, part = "all") %>% 
    autofit(part = 'all') %>%
    width(width = 7.4/3) 

print(tb)

# Supplementary Table -----------------------------------------------------

tb <- tableone::CreateTableOne(
  vars       = c('sex', 'bl.age','aoo', 'gaa1', 'gaa2', 'pm.yn', 'mFARS', 'FARS.E','FARS.B','FARS.C','FARS.Am'),
  factorVars = c('sex','bl.amb', 'pm.yn'),
  strata     = 'strata',
  data       = dm. %>% 
    filter( study  == 'FACHILD' ) %>% 
    mutate( strata  = ifelse(itt, 'Analysis Population', ifelse(bl.amb=='non-amb.', 'Non-Ambulatory', 'Other'))) %>% 
    mutate( stata   = factor(strata, c('Analysis Population', 'Other', 'Non-Ambulatory')))
) %>% 
  print(
    varLabels = T,
    nonnormal = c('aoo','gaa1'),
    contDigits = 1, catDigits = 1,
    missing = F,
    test = F,
    explain = F, dropEqual= F,
    add.rownames = T, noSpaces = T) %>%
  data.frame() %>% rownames_to_column() %>%
  flextable() %>% 
  fontsize(size = 14, part = "all") %>% 
  autofit(part = 'all') %>%
  align(align = 'center', j = 2:4, part = 'all' ) %>% 
  width(width = 1.8*1.0284) 

print(tb)

read_pptx("../Templates/CR.template.pptx") %>%
  add_slide ( layout = 'TTE', master = 'CR') %>%
  ph_with ( 'Demographics', location = ph_location_type(type = "title")) %>%
  ph_with ( tb , location = ph_location_type(type = "body", id = 1)) %>%
  print   ('Demographics Table.pptx')


# .tab1.sub( dt.bl, strata = c('s.group.age') ) %>% 
#   save_as_html(path = 'med.age.pptx')
# 
# .tab1.sub( dt.bl, strata = c('s.group.mF') ) %>% 
#   save_as_pptx(path = 's.group.mF.pptx')
# 
# .tab1.all( dt.bl ) %>% 
#   save_as_pptx(path = 'demo.all.pptx')
# 
# 
# .tab1.sub( dt.bl %>% filter(group == 'ok'), strata = c('group') )
# 
# .tab1.sub( 
#   dt.bl %>% 
#     filter(group == 'ok'), strata = c('study') )
# .tab1.sub( 
#   dt.bl %>% 
#     filter(group == 'ok'), strata = 'group' )
# 
# 
# 
# read_pptx( '../Templates/CR.template.pptx' ) %>%
#   add_slide   ( layout = 'TTE', master = 'CR') %>%
#   ph_with     ( .tab1.all(dt.bl                    ), location = ph_location_type( type = "body" , id = 1) ) %>%
#   add_slide   ( layout = 'TTE', master = 'CR') %>%
#   ph_with     ( .tab1.sub(dt.bl, strata = 'study'), location = ph_location_type( type = "body" , id = 1) ) %>%
#   add_slide   ( layout = 'TTE', master = 'CR') %>%
#   ph_with     ( .tab1.sub(dt.bl, strata = c('study','amb')), location = ph_location_type( type = "body" , id = 1) ) %>%
#   add_slide   ( layout = 'TTE', master = 'CR') %>%
#   ph_with     ( .tab1.sub(dt.bl, strata = c('study','med.age')), location = ph_location_type( type = "body" , id = 1) ) %>%
#   add_slide   ( layout = 'TTE', master = 'CR') %>%
#   ph_with     ( .tab1.sub(dt.bl, strata = c('study','med.FrE')), location = ph_location_type( type = "body" , id = 1) ) %>%
#   print ( target = paste('Demo.Tables.', gsub(":","-", Sys.time()), ".pptx", sep="") )





