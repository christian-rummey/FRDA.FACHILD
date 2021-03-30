
# setup ---------------------------------------------------------------

rm(list=ls())
source('list.pars.R')

theme_set( 
  theme_minimal(base_size = 14)+
    theme(panel.grid.minor = element_blank())+
    .box
    )

# outcomes selection ------------------------------------------------------

params <- params.[c(8)]

# datasets ----------------------------------------------------------------

# readRDS( 'DATA derived/dt.coef.slope.rds')
# readRDS( 'DATA derived/dt.coef.aval.rds' )
# readRDS( 'DATA derived/dt.coef.chg.rds'  )
est.chg <- readRDS( 'DATA derived/dt.est.chg.rds'   )

# plots -------------------------------------------------------------------

.long.plot.MMRM <- function (df, parid) {
  
  title = paste(params.[parid], '- Estimated Changes (MMRM)')
  
  mxmn <- est.chg %>% 
    filter( param %in% params.[parid]) %>%
    group_by(param) %>% 
    summarise(
      mx = max(conf.high, na.rm=T),
      mn = min(conf.low , na.rm=T)
    ) %>% 
    select(param, mx, mn) %>% unique 
  
  df %>% 
    filter( param %in% params.[parid]) %>% 
    filter( group == 'all' ) %>% 
    ggplot()+geom_pointrange()+geom_line()+
    aes( avisit, estimate, ymin = conf.low, ymax = conf.high)+
    aes( group = param )+
    # geom_text ( aes(label = n , y = height), size = 3 )+
    geom_hline( yintercept = 0, linetype = 'dashed')+
    facet_wrap( ~ param, scale = 'free_y')+
    scale_x_discrete(labels = c('BL', '6m', '1y', '18m', '2y'))+
    .leg_ll+
    xlab('Visit')+
    ylab('Estimated Change (95%CI)')+
    ggtitle(title)+
    coord_cartesian(ylim = c(mxmn$mn, mxmn$mx))
  }

.long.plot.MMRM.sub <- function (df, groups, parid, title) {
  
  .dodge <- position_dodge(width = 0.1)
  
  mxmn <- est.chg %>% 
    filter( param %in% params.[parid]) %>%
    group_by(param) %>% 
    summarise(
      mx = max(conf.high, na.rm=T),
      mn = min(conf.low , na.rm=T)
    ) %>% 
    select(param, mx, mn) %>% unique 
  
  df %>% 
    filter( param %in% params.[parid]) %>% 
    filter( group %in% groups ) %>% 
    ggplot()+geom_pointrange(position = .dodge)+geom_line(position = .dodge)+
    aes( avisit, estimate, ymin = conf.low, ymax=conf.high)+
    aes( shape = group )+ scale_shape_manual(values = c(21,19))+
    aes( group = group)+
    # geom_text ( aes(label = n , y = height), size = 3,position = position_dodge(width = 0.6) )+
    geom_hline( yintercept = 0, linetype = 'dashed')+
    facet_wrap( ~ param, scale = 'free_y')+
    scale_x_discrete(labels = c('BL', '6m', '1y', '18m', '2y'))+
    .leg_lr+
    xlab('Visit')+
    ylab('Estimated Change (95%CI)')+
    ggtitle(title)+
    coord_cartesian(ylim = c(mxmn$mn, mxmn$mx))
  
  }

p1 <- .long.plot.MMRM    (est.chg, c(10))
p2 <- .long.plot.MMRM.sub(est.chg, c('ambulatory','non-amb.')         ,c(10), 'mFARS by Ambulation, Estimated Changes (MMRM)')
p3 <- .long.plot.MMRM.sub(est.chg, c('FARS.E < 22.8','FARS.E > 22.8') ,c(10), 'mFARS by Median FARS E, Estimated Changes (MMRM)')
p4 <- .long.plot.MMRM.sub(est.chg, c('age < 13.9','age > 13.9')       ,c(10), 'mFARS by Median Age, Estimated Changes (MMRM)')

# tables ------------------------------------------------------------------

ft1 <- est.chg %>% 
  filter(avisit != 'BL') %>% 
  # filter( param %in% params.[parid]) %>% 
  # filter( group %in% groups ) %>% 
  filter( param %in% 'mFARS') %>% 
  filter( group %in% c('age < 13.9', 'age > 13.9') ) %>% 
  select(param, group, avisit, estimate, conf.low, conf.high, p.value) %>% 
  mutate_at('p.value', ~ sprintf('%.4f', .)) %>% 
  mutate_at(c('estimate', 'conf.high', 'conf.low'), ~ sprintf('%.1f', .)) %>% 
  mutate(result = paste(estimate, '\n', conf.low, 'to', conf.high, '\n', p.value)) %>% 
  select(param, group, avisit, result) %>% 
  spread(avisit, result) %>% 
  flextable

ft1 %<>% 
  theme_alafoli() %>% 
  delete_part( part = "header" ) %>% 
  align(align = 'center', part = 'all') %>% 
  width( c(1,2), width = 1.9/2) %>% 
  width( c(3,4,5,6), width = (7.4-1.9)/4)

# output ------------------------------------------------------------------

read_pptx ( '../Templates/CR.template.pptx' ) %>%
  # add_slide ( layout = 'TTEsplit', master = 'CR')  %>%
  # ph_with   ( dml ( print ( 
  #   p1,
  #   newpage = F ) ), location = ph_location_type( type = "body" , id = 3) ) %>%
  # add_slide ( layout = 'TTEsplit', master = 'CR')  %>%
  # ph_with   ( dml ( print ( 
  #   p2,
  #   newpage = F ) ), location = ph_location_type( type = "body" , id = 3) ) %>%
  # add_slide ( layout = 'TTEsplit', master = 'CR')  %>%
  # ph_with   ( dml ( print (
  #   p3,
  #   newpage = F ) ), location = ph_location_type( type = "body" , id = 3) ) %>%
  add_slide ( layout = 'TTEsplit', master = 'CR')  %>%
  ph_with   ( dml ( print (
    p4,
    newpage = F ) ), location = ph_location_type( type = "body" , id = 3) ) %>%
  ph_with   ( ft1 , location = ph_location_type( type = "body" , id = 2) ) %>%
  print ( target = paste('FARS.E.Changes.MMRM.', gsub(":","-", Sys.time()), ".pptx", sep="") )
