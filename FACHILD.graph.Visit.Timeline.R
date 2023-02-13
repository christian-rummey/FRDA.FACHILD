
# beware mFARS / NO mFARS ----------------------------------------------------
# when there is more than one visit.date, favor mFARS dates

dt.   <- readRDS('DATA derived/dt.rds') %>%
  select( study, sjid, adt, avisitn, time., paramcd, aval) %>% 
  spread( paramcd, aval) %>%
  mutate( window.dev = abs( time. - avisitn )) %>% 
  mutate( dev.cat = cut(window.dev, c(0, 0.25, 0.5, 1, 10), labels = c('<3M', '>3M', '>6M', '>1y'), include.lowest = T ) ) %>%  
  select(-c('FARS.E', 'FARS.B', 'FARS.C','FARS.Am', 'w25.i', 'hpt.id', 'hpt.in', 'hpt.i', 'tug.i', 'w1m', 'w6m', 'bbs', 'ADL' )) %>% 
  mutate( mFARS = ifelse(is.na(mFARS), F, T)) %>% 
  arrange(sjid, avisitn)

dt. %>%
  group_by ( study ) %>% 
  summarise(
    sjids        = length( unique(        sjid          )),
    visits       = length( unique( paste( sjid, avisitn))),
    visits.mFARS = length( !mFARS )
  ) 

dt. %>% 
  ungroup %>% select(mFARS, study) %>% table

# add site ----------------------------------------------------------------

dt. %<>% 
  group_by( sjid ) %>% 
  mutate  ( startdt = min(adt) ) %>% 
  left_join(.dd.atx('demo') %>% select(sjid, site))

# sort sjid by start date -------------------------------------------------

dt.$sjid <- fct_reorder(dt.$sjid, dt.$startdt, min)

# virt.vis <- dt. %>%
#   ungroup %>% 
#   # mutate(hpf = ifelse( is.na(hpf), 'filler', as.character(hpf))) %>% 
#   select( -fpf ) %>% 
#   filter  ( !grepl('.iu', paramcd) ) %>% 
#   select  ( study, sjid, adt, paramcd, hpf) %>% 
#   group_by( study, sjid, adt, paramcd) %>%
#   filter  ( hpf %in% c('Audio Only','Video')) %>% 
#   ungroup %>% 
#   select  ( -paramcd, -hpf) %>% 
#   unique %>% 
#   arrange( adt ) %>% 
#   # filter (!(sjid == 5035 & adt == as.Date('2019-02-25'))) %>% 
#   mutate(virtual = 'virtial') %>% 
#   droplevels

dt. %>%
  group_by(study, sjid, avisitn) %>% 
  mutate( n.visit.dates = n() )  %>% 
  filter( !(n.visit.dates > 1 & !mFARS) ) %>% # this graph can only be made for one param
  filter(study == 'FACHILD') %>% 
  ggplot()+
  lemon::geom_pointline( shorten = .1 ) +
  aes ( x = adt, y = sjid)+
  aes ( group = sjid, color = site)+
  facet_wrap(~study)+
  theme_minimal(base_size = 16)+
  theme( panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank())+
  theme( axis.text.y = element_blank())+.leg_tl+
  xlab ( 'Date' )+ ylab('Subjects')+
  ggtitle('Follow-up Timeline')+theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept = as.Date('2020-03-01', linetype = 2))+
  geom_vline(xintercept = as.Date('2019-10-08', linetype = 3))

dt.tmp <- dt. %>%
  group_by(study, sjid, avisitn) %>% 
  mutate( n.visit.dates = n() )  %>% 
  filter( !(n.visit.dates > 1 & !mFARS) ) %>% # this graph can only be made for one param
  filter( study   == 'FACHILD' )

dt.tmp %>%
  group_by(sjid) %>% 
  filter( max(window.dev)>0.25 ) %>% 
  mutate( avisitn = ifelse(time. < 2.25, round(time.*2)/2, round(time.) ) ) %>%
  mutate( window.dev = abs( time. - avisitn )) %>% 
  mutate( dev.cat = cut(window.dev, c(0, 0.25, 0.5, 1, 10), labels = c('<3M', '>3M', '>6M', '>1y'), include.lowest = T ) ) %>%  
  group_by(study, sjid, avisitn) %>% 
  filter( window.dev == min(window.dev) ) %>% 
  ggplot()+
  lemon::geom_pointline( shorten = .1) +
  aes(x = time., y = sjid)+
  aes(group = sjid, color = dev.cat)+scale_color_manual(values = c('grey','#377eb8','#e41a1c'))+
  aes(shape = mFARS)+scale_shape_manual(values = c(21,19))+
  facet_wrap(~study)+
  theme_minimal(base_size = 16)+
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank())+
  # theme(axis.text.y = element_blank())+#.leg_tr+
  xlab('Date')+ylab('Subjects')+
  ggtitle('Follow-up Timeline')+theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept = c( 0.5, 1, 1.5, 2, 3, 4))+
  geom_vline(xintercept = c( 0.25, 0.75, 1.25, 1.75, 2.25, 2.75, 3.75), linetype =3)+
  .leg_tr

# statistics --------------------------------------------------------------

dt. %>% 
  filter(mFARS) %>% ungroup %>% 
  select(study, dev.cat) %>% 
  table

dt. %>% 
  ungroup %>% 
  select(study, dev.cat) %>% 
  table


# adjustment --------------------------------------------------------------
# recalculate visit times and windows

# any dev > 3M ------------------------------------------------------------

dt. %>%
  group_by(sjid) %>% filter(max(window.dev)>.25) %>% 
  filter( study   == 'FACHILD' ) %>% 
  ggplot()+
  lemon::geom_pointline( shorten = .1) +
  aes(x = time., y = sjid)+
  aes(group = sjid, color = dev.cat)+scale_color_manual(values = c('grey','#377eb8','#e41a1c'))+
  aes(shape = mFARS)+scale_shape_manual(values = c(21,19))+
  facet_wrap(~study)+
  theme_minimal(base_size = 16)+
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank())+
  xlab('Date')+ylab('Subjects')+
  ggtitle('Follow-up Timeline')+theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept = c( 0.5, 1, 1.5, 2, 3, 4))+
  geom_vline(xintercept = c( 0.25, 0.75, 1.25, 1.75, 2.25, 2.75, 3.25, 3.75), linetype =3)

# adjusting selectively ------------------------------------------------------------
# and remove duplicates.

dt.adj <- dt. %>%
  filter(study == 'FACHILD') %>% 
  group_by(study, sjid) %>% filter(max(window.dev)>.25) %>% 
  mutate( avisitn    = ifelse(time. < 2.25, round(time.*2)/2, round(time.) ) ) %>%
  mutate( window.dev = abs( time. - avisitn ) ) %>%
  mutate( dev.cat = cut(window.dev, c(0, 0.25, 0.5, 1, 10), labels = c('<3M', '>3M', '>6M', '>1y'), include.lowest = T ) ) %>%
  group_by(study, sjid, avisitn) %>% 
  filter( window.dev == min(window.dev))

dt.adj %>% 
  ggplot()+
  lemon::geom_pointline( shorten = .1) +
  aes(x = time., y = sjid)+
  aes(group = sjid, color = dev.cat)+scale_color_manual(values = c('grey','#377eb8','#e41a1c'))+
  aes(shape = mFARS)+scale_shape_manual(values = c(21,19))+
  facet_wrap(~study)+
  theme_minimal(base_size = 16)+
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank())+
  xlab('Date')+ylab('Subjects')+
  ggtitle('Follow-up Timeline')+theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept = c( 0.5, 1, 1.5, 2, 3, 4))+
  geom_vline(xintercept = c( 0.25, 0.75, 1.25, 1.75, 2.5, 3.5), linetype =3)

# .sp()

# additionally adjust >2.25y ----------------------------------------------

dt.adj %>% 
  group_by( sjid ) %>% filter(max (window.dev)>0.25) %>% 
  # filter  ( time. >= 2.25 & time. < 3.5 ) %>%
  mutate  ( dev.cat = as.character(dev.cat)) %>% 
  mutate  ( dev.cat = ifelse(dev.cat == '>3M' & time. >  2.25 & time. < 2.5 , '>3M.ok', dev.cat )) %>%
  mutate  ( dev.cat = ifelse(dev.cat == '>3M' & time. >= 2.5  & time. < 3.5 , '>3M.ok', dev.cat )) %>%
  select ( -vname ) %>% .vnames %>%
  mutate  ( dev.cat = factor(dev.cat, c(levels(dt.adj$dev.cat), '>3M.ok'))) %>% 
  # group_by(study, sjid, avisitn) %>% 
  # filter(n()>1)
  ggplot()+
  lemon::geom_pointline( shorten = .1) +
  aes(x = time., y = sjid)+
  aes(group = sjid, color = dev.cat)+scale_color_manual(values = c('grey','#e41a1c','#377eb8'))+
  aes(shape = mFARS)+scale_shape_manual(values = c(21,19))+
  facet_wrap(~study)+
  theme_minimal(base_size = 16)+
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank())+
  xlab('Date')+ylab('Subjects')+
  # geom_text(aes(label = avisitn))+
  ggtitle('Follow-up Timeline')+theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept = c( 0.5, 1, 1.5, 2, 3, 4))+
  geom_vline(xintercept = c( 0.25, 0.75, 1.25, 1.75, 2.5, 3.5), linetype =3)

# .sp()