

# read data ---------------------------------------------------------------
rm(list = ls())

require(labelled)

dt.   <- readRDS('DATA derived/dt.rds')
demo. <- .dd.FA('demo') %>% filter( sjid %in% dt.$sjid )

options(digits = 5)

# . -----------------------------------------------------------------------
# 12 patients that were unable at something at baseline. used as empty
# first age/dur during a visit is used (sometimes bbs comes from later)

dt. %<>% 
  # filter( unable == F) %>%
  left_join(demo. %>% select(sjid, sex)) %>% 
  rename( med.age = age.groups ) %>% 
  select( sjid, age, sex, avisitn, symp, gaa1, pm, step, med.age, fds, amb, paramcd, aval) %>%
  group_by ( sjid ) %>% 
  arrange(sjid, avisitn, paramcd) %>% 
  mutate( 
    fu       = max(age)-min(age),
    fu_v     = max(avisitn)-1,
    age      = min(age)
    ) %>%
  group_by ( sjid, avisitn ) %>%
  # filter(!(paramcd %in% c('FARS.Am', 'FARS.C'))) %>% 
  spread( paramcd , aval ) %>% 
  mutate(  ) %>% 
  group_by( sjid ) %>% 
  filter( avisitn == min(avisitn) ) %>% 
  ungroup

dt. %>% filter(n()>1)

med.FARS.E <- round(median(dt.$FARS.E, na.rm=T))
dt. %<>% 
  ungroup %>% 
  mutate(med.FARS.E = ifelse(FARS.E < med.FARS.E, '<23', '>=23')) %>% 
  mutate( pm = ifelse(pm == 0, 0, 1))

var_label(dt.) <- list(symp = 'Age of Onset', pm = 'Point Mutation', gaa1 = 'Repeat Length (short)',
                      med.age = 'Median Age',
                      sex = 'Sex',
                      age  = 'Age (BL)', 
                      fu = 'Follow Up (years)',
                      fu_v = 'Follow Up (visits)')

# demo.table all --------------------------------------------------------------
# filter(dt., age > median(dt.$age))

.tab1.all <- function ( df )  {
  tb <- tableone::CreateTableOne(
    vars       = c('sex','symp', 'gaa1', 'pm', 'age', 'fu_v', 'amb', 'mFARS', 'FARS.E','ADL','bbs'),
    factorVars = c('sex','amb', 'pm'),
    test = F,
    data       = df
  ) %>% 
    print(
      varLabels = T,
      nonnormal = c('age','mx.age','dur','symp','gaa1','fu','fu_v'),
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
    vars       = c('sex','symp', 'gaa1', 'pm', 'age', 'fu_v', 'amb', 'mFARS', 'FARS.E','ADL','bbs'),
    factorVars = c('sex','amb', 'pm'),
    strata     = strata,
    test = F,
    data       = df
  ) %>% 
    print(
      varLabels = T,
      nonnormal = c('age','mx.age','dur','symp','gaa1','fu','fu_v'),
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
  ph_with     ( .tab1.all(dt.                    ), location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide   ( layout = 'TTE', master = 'CR') %>%
  ph_with     ( .tab1.sub(dt., strata = 'amb'), location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide   ( layout = 'TTE', master = 'CR') %>%
  ph_with     ( .tab1.sub(dt., strata = 'med.age'), location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide   ( layout = 'TTE', master = 'CR') %>%
  ph_with     ( .tab1.sub(dt., strata = 'med.FARS.E'), location = ph_location_type( type = "body" , id = 1) ) %>%
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


