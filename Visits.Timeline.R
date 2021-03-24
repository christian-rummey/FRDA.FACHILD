
dt.   <- readRDS('DATA derived/dt.long.rds')

dt. %<>% 
  group_by(sjid) %>% 
  mutate(startdt = min(adt)) %>% 
  left_join(.dd.FA('demo') %>% select(sjid, site))

dt.$sjid <- fct_reorder(dt.$sjid, dt.$startdt, min)

dt. %>% filter()

virt.vis <- dt. %>%
  ungroup %>% 
  # mutate(hpf = ifelse( is.na(hpf), 'filler', as.character(hpf))) %>% 
  select( -fpf ) %>% 
  filter  ( !grepl('.iu', paramcd) ) %>% 
  select  ( study, sjid, adt, param, hpf) %>% 
  group_by( study, sjid, adt, param) %>%
  filter  ( hpf %in% c('Audio Only','Video')) %>% 
  ungroup %>% 
  select  ( -param, -hpf, -param) %>% 
  unique %>% 
  arrange( adt ) %>% 
  # filter (!(sjid == 5035 & adt == as.Date('2019-02-25'))) %>% 
  mutate(virtual = 'virtial') %>% 
  droplevels

p1 <- dt. %>%
  ggplot()+
  aes(x = adt, y = sjid)+
  aes(group = sjid, color = site)+
  # aes(shape = virtual)+scale_shape_manual(values=c(21,19))+
  # scale_y_discrete()+
  geom_point()+geom_line()+
  theme_minimal(base_size = 16)+
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank())+
  theme(axis.text.y = element_blank())+.leg_tl+
  xlab('Date')+ylab('Subjects')+
  ggtitle('Follow-up Timeline')+theme(plot.title = element_text(hjust = 0.5))

p1

p2 <- dt. %>%
  left_join(virt.vis) %>% mutate(virtual = ifelse(is.na(virtual), 'In-Person', virtual)) %>% 
  ggplot()+
  aes(x = adt, y = sjid)+
  aes(group = sjid, color = site)+
  aes(shape = virtual)+scale_shape_manual(values=c(21,19))+
  # scale_y_discrete()+
  geom_point()+geom_line()+
  theme_minimal(base_size = 16)+
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank())+
  theme(axis.text.y = element_blank())+.leg_tl+
  xlab('Date')+ylab('Subjects')+
  ggtitle('Follow-up Timeline')+theme(plot.title = element_text(hjust = 0.5))

p2

# . -----------------------------------------------------------------------

read_pptx( '../Templates/CR.template.pptx' ) %>%
  add_slide   ( layout = 'TTE', master = 'CR') %>%
  ph_with     ( p1, location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide   ( layout = 'TTE', master = 'CR') %>%
  ph_with     ( p2, location = ph_location_type( type = "body" , id = 1) ) %>%
  print ( target = paste('Visits.Timeline.', gsub(":","-", Sys.time()), ".pptx", sep="") )