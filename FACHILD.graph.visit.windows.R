
dt. <- .dd.atx('visit.dates.FACHILD', c = T) %>% 
  mutate(avisit = factor(avisit, levels = c("BL","V02","V03","V04","V05","V06","V07"))) %>% 
  select(study, sjid, avisit, avisitn, adt, flagged) %>% 
  unique

dt. %>% filter(flagged == T)


# for visit stats, use best option ----------------------------------------

dt. %<>%
  group_by(sjid) %>% 
  mutate ( time.      = as.numeric(adt-min(adt))/365.25) %>% 
  mutate ( window.dev = abs(time.-avisitn) ) %>% 
  group_by( study, sjid, avisitn ) %>% 
  filter( window.dev == min(window.dev) )

# all but one (4961) with V07, have a suitable V06 ------------------------
# ...and that one is at 3.8 years. 

sjids.tmp <- dt. %>% ungroup %>% filter(avisit == 'V07') %>% select(sjid) %>% deframe
dt. %>% 
  filter( sjid %in% sjids.tmp ) %>% 
  filter( avisitn >= 3)

# Visit % -----------------------------------------------------------------

dt. %>% 
  ungroup %>% 
  mutate(N   = length(unique(sjid))) %>% 
  group_by( study, avisit, avisitn, N ) %>%
  count() %>% 
  mutate(pct = round(100*n/(N), 0)) %>% 
  ggplot()+geom_col(position = position_stack(reverse = TRUE))+
  # aes(x = avisitn)+scale_x_continuous(breaks = c(0, .5, 1, 1.5, 2, 3, 4), labels = levels(dt.$avisit)[c(1:7)])+
  aes(x = avisitn)+scale_x_continuous(breaks = c(0, .5, 1, 1.5, 2, 3, 4), labels = c('BL', '6M','1y','18M','2y','3y','3y+'))+
  aes(y = pct)+
  geom_text(aes(label = paste(n,'\n(',pct,'%)', sep = '')), nudge_y = -5, color = 'white')+
  theme_minimal(base_size = 20)+theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank())+
  .leg_tr+
  xlab('Visit Time')+
  ylab('Percentage of Subjects')

# .sp()

# Visit Windows  -------------------------------------------------------------------

dt.tmp <- dt. %>%   
  mutate ( dev.cat = cut(window.dev, c(0, 0.25, 0.5, 1, 10), labels = c('<3M', '>3M', '>6M', '>1y'), 
                       include.lowest = T), ) %>% 
  group_by( avisit) %>% 
  mutate ( N   = length(unique(sjid))) %>% 
  group_by( study, avisit, avisitn, dev.cat, N ) %>%
  count() %>% 
  mutate(pct = round(100*n/(N), 0))

dt.tmp %>% 
  ggplot()+geom_col(position = position_stack(reverse = TRUE))+
  aes(x = avisitn)+scale_x_continuous(breaks = c(0, .5, 1, 1.5, 2, 3, 4), labels = c('BL', '6M','1y','18M','2y','3y','3y+'))+
  # aes(x = avisitn)+scale_x_continuous(breaks = c(0, .5, 1, 1.5, 2, 3, 4), labels = levels(dt.$avisit)[c(1:7)])+
  aes(y = n)+scale_y_continuous(breaks = seq(0, 100, 25))+
  aes(fill = dev.cat)+scale_fill_brewer(palette = 'Set1', direction = -1)+
  geom_text(aes(label = paste(n,'\n(',pct,'%)', sep = '')), nudge_y = -5, color = 'black', 
            data = filter(dt.tmp, dev.cat == '<3M'))+
  theme_minimal(base_size = 20)+theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank())+
  .leg_tr+
  xlab('Visit Time')+
  ylab('Number of Subjects')

# .sp()  
