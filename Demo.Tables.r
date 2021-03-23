

# read data ---------------------------------------------------------------
rm(list = ls())

require(labelled)
source ('list.pars.R')

dt.bl   <- readRDS('DATA derived/dt.bl.rds')

options(digits = 5)


# demo.table all --------------------------------------------------------------

.tab1.all <- function ( df )  {
  tb <- tableone::CreateTableOne(
    vars       = c('sex','symp', 'gaa1', 'pm', 'bl.age', 'fu_v', 'amb', 'mFARS', 'FARS.E','ADL','bbs'),
    factorVars = c('sex','amb', 'pm'),
    test = F,
    data       = df
  ) %>% 
    print(
      varLabels = T,
      nonnormal = c('bl.age', 'mx.age','dur','symp','gaa1','fu','fu_v'),
      contDigits = 1, catDigits = 0,
      # missing = T,
      explain = F, dropEqual= F,
      add.rownames = T) %>%
    data.frame() %>% rownames_to_column() %>%
    flextable() %>% 
    width(width = 7.4/3) %>% 
    fontsize(size = 18, part = "all") %>% 
    height_all(7.10/13, part = "all") 
  return (tb)
}
.tab1.sub <- function (df, strata = NA )  {
  tb <- tableone::CreateTableOne(
    vars       = c('sex','symp', 'gaa1', 'pm', 'bl.age', 'fu_v', 'amb', 'mFARS', 'FARS.E','ADL','bbs'),
    factorVars = c('sex','amb', 'pm'),
    strata     = strata,
    test = F,
    data       = df
  ) %>% 
    print(
      varLabels = T,
      nonnormal = c('bl.age', 'mx.age','dur','symp','gaa1','fu','fu_v'),
      contDigits = 1, catDigits = 0,
      # missing = T,
      explain = F, dropEqual= F,
      add.rownames = T) %>%
    data.frame() %>% rownames_to_column() %>%
    flextable() %>% 
    width(width = 7.4/3) %>% 
    fontsize(size = 18, part = "all") %>% 
    height_all(7.10/13, part = "all") 
  return (tb)
}

read_pptx( '../Templates/CR.template.pptx' ) %>%
  add_slide   ( layout = 'TTE', master = 'CR') %>%
  ph_with     ( .tab1.all(dt.bl                    ), location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide   ( layout = 'TTE', master = 'CR') %>%
  ph_with     ( .tab1.sub(dt.bl, strata = 'amb'), location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide   ( layout = 'TTE', master = 'CR') %>%
  ph_with     ( .tab1.sub(dt.bl, strata = 'med.age'), location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide   ( layout = 'TTE', master = 'CR') %>%
  ph_with     ( .tab1.sub(dt.bl, strata = 'med.FARS.E'), location = ph_location_type( type = "body" , id = 1) ) %>%
  print ( target = paste('Demo.Tables.', gsub(":","-", Sys.time()), ".pptx", sep="") )


# # flextable ---------------------------------------------------------------
# std_border = officer::fp_border(style = 'dotted', width = 1)
# 
# ft %>%
#   as.data.frame() %>%
#   # rownames_to_column('names') %>%
#   flextable( ) %>%
#   theme_booktabs(fontsize = 12) %>%
#   align(align = "left", part = "all") %>%
#   align(j = 2:4, align = "center", part = "all") %>%
#   # colformat_num(c('estimate')) %>%
#   # color ('grey', j = c('term',,'p'), i = ~ p.value >= 0.01) %>%
#   hline(i = seq(3,nrow(ft),3), border = std_border) %>%
#   # merge_v(j = c('param')) %>%
#   autofit()
# 



# 
# .st <- function(l = "TTE", ti = "(st, no title)", tt = ft, template = "../Templates/CR.template.pptx", m = "CR", i = 1){
# 
#   read_pptx( template ) %>%
#     add_slide   ( layout = l, master = m) %>%
#     # ph_with (tt, location = ph_location_type( type = "body" , id = i ) ) %>%
#     ph_with (tt, location = ph_location_type( type = "body" , id = i) ) %>%
#     print ( target = paste(gsub(":","-", Sys.time()), " - ", ti,".pptx", sep="") )
# 
#   rm(tt)
# 
# }
# 
# .st()


