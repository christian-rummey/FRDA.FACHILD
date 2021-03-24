
dt.   <- readRDS('DATA derived/dt.long.rds')

dt. %<>% 
  group_by(sjid) %>% 
  mutate(startdt = min(adt)) %>% 
  left_join(.dd.FA('demo') %>% select(sjid, site))

dt.$sjid <- fct_reorder(dt.$sjid, dt.$startdt, min)

p <- dt. %>%
  ggplot()+
  aes(x = adt, y = sjid)+
  aes(group = sjid, color = site)+
  # scale_y_discrete()+
  geom_point()+geom_line()+
  theme_minimal(base_size = 16)+
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank())+
  theme(axis.text.y = element_blank())+.leg_tl+
  xlab('Date')+ylab('Subjects')+
  ggtitle('Follow-up Timeline')+theme(plot.title = element_text(hjust = 0.5))

read_pptx( '../Templates/CR.template.pptx' ) %>%
  add_slide   ( layout = 'TTE', master = 'CR') %>%
  ph_with     ( p, location = ph_location_type( type = "body" , id = 1) ) %>%
  print ( target = paste('Visits.Timeline.', gsub(":","-", Sys.time()), ".pptx", sep="") )