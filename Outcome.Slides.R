
# setup ---------------------------------------------------------------

rm(list=ls())
source('list.pars.R')

theme_set( 
  theme_minimal(base_size = 14)+
    theme(panel.grid.minor = element_blank())+
    .box
    )

# outcomes selection ------------------------------------------------------

params.

parid <- 5

param <- params.[c(parid)]

groups   <- c("all", "ambulatory", "non-amb.", "age < 13.9", "age > 13.9", "FARS.E < 22.8", "FARS.E > 22.8")
sgs.a    <- groups[c(1)]
sgs.1    <- groups[c(2,3)]
sgs.2    <- groups[c(4,5)]
sgs.3    <- groups[c(6,7)]

# datasets ----------------------------------------------------------------

# readRDS( 'DATA derived/dt.coef.slope.rds')
# readRDS( 'DATA derived/dt.coef.aval.rds' )
# readRDS( 'DATA derived/dt.coef.chg.rds'  )
est.chg <- readRDS( 'DATA derived/dt.est.chg.rds'   )

# plots -------------------------------------------------------------------

.plot <- function (df, parid) {
  
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
    # ggtitle(title)+
    coord_cartesian(ylim = c(mxmn$mn, mxmn$mx))
  }

.plot.sub <- function (df, groups, parid) {
  
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
    # ggtitle(title)+
    coord_cartesian(ylim = c(mxmn$mn, mxmn$mx))
  
  }

# p1 <- .long.plot.MMRM    (est.chg, parid)
# p2 <- .long.plot.MMRM.sub(est.chg, c('ambulatory','non-amb.')         ,parid)
# p3 <- .long.plot.MMRM.sub(est.chg, c('FARS.E < 22.8','FARS.E > 22.8') ,parid)
# p4 <- .long.plot.MMRM.sub(est.chg, c('age < 13.9','age > 13.9')       ,parid)

# tables ------------------------------------------------------------------

.table <- function (df, parid, groups = 'all') {
  
  if (pars.[parid] %in% c('w6m')) {
      df %<>%
        mutate_at(c('estimate', 'conf.high', 'conf.low'), ~ sprintf('%.0f', .))
    } else if (pars.[parid] %in% c('hpt.i','w25.i', 'tug.i')) {
      df %<>%
        mutate_at(c('estimate', 'conf.high', 'conf.low'), ~ sprintf('%.3f', .))
    } else {
      df %<>%
        mutate_at(c('estimate', 'conf.high', 'conf.low'), ~ sprintf('%.1f', .))
      }
  
  ft <- df %>%
    filter( avisit != 'BL') %>%
    filter( param %in% params.[parid]) %>%
    filter( group %in% groups ) %>%
    select( param, group, avisit, estimate, conf.low, conf.high, p.value) %>%
    mutate_at('p.value', ~ sprintf('%.4f', .)) %>%
    mutate(result = paste(estimate, '\n', conf.low, 'to', conf.high, '\np=', p.value)) %>%
    select(param, group, avisit, result) %>%
    spread(avisit, result) %>%
    flextable() %>% 
    border_remove() %>% 
    delete_part( part = "header" ) %>%
    merge_at( j = c(1) ) %>% 
    align(align = 'center', part = 'all') %>% 
    width( c(1,2), width = 1.9/2) %>% 
    width( c(3,4,5,6), width = (7.1-1.8)/4) %>% 
    height_all( height = 1.69/2) %>% 
    line_spacing(space = 1, part = "body")

  if (length(groups)>1){
    
    ft %<>% 
      hline(i = 1, j = c(2:6), part = 'body', border = fp_border(width = .5))
      
  }

  return (ft)
}

# output ------------------------------------------------------------------

read_pptx ( '../Templates/CR.template.pptx' ) %>%
  add_slide ( layout = 'TTEsplit', master = 'CR')  %>%
  ph_with   ( dml ( print (
    .plot (est.chg, parid),
    newpage = F ) ), location = ph_location_type( type = "body" , id = 3) ) %>%
  ph_with ( 
    .table( est.chg, parid),
    location = ph_location_type( type = "body" , id = 2) ) %>%
  add_slide ( layout = 'TTEsplit', master = 'CR')  %>%
  ph_with   ( dml ( print (
    .plot.sub (est.chg, sgs.1, parid),
    newpage = F ) ), location = ph_location_type( type = "body" , id = 3) ) %>%
  ph_with ( 
    .table( est.chg, parid, sgs.1),
    location = ph_location_type( type = "body" , id = 2) ) %>%
  add_slide ( layout = 'TTEsplit', master = 'CR')  %>%
  ph_with   ( dml ( print (
    .plot.sub (est.chg, sgs.2, parid),
    newpage = F ) ), location = ph_location_type( type = "body" , id = 3) ) %>%
  ph_with ( 
    .table( est.chg, parid, sgs.2),
    location = ph_location_type( type = "body" , id = 2) ) %>%
  add_slide ( layout = 'TTEsplit', master = 'CR')  %>%
  ph_with   ( dml ( print (
    .plot.sub (est.chg, sgs.3, parid),
    newpage = F ) ), location = ph_location_type( type = "body" , id = 3) ) %>%
  ph_with ( 
    .table( est.chg, parid, sgs.3),
    location = ph_location_type( type = "body" , id = 2) ) %>%
  print ( target = paste('Changes.', gsub("[[:punct:]]", "", param) , '.' ,gsub(":","-", Sys.time()), ".pptx", sep="") )



