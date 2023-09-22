
# has mixed visit labels (17) ---------------------------------------------

dt. <- .dd.atx('../DATA/FACOMS/2023-06-02/derived/visit.dates.FACHILD', c = T) %>% 
  mutate(avisit = factor(avisit, levels = c("BL","V02","V03","V04","V05","V06","V07"))) %>% 
  select(study, sjid, avisit, avisitn, adt, hpf, fpf, flagged) %>% 
  unique

# correct -----------------------------------------------------------------

dt. <- .dd.atx('../DATA/FACOMS/2023-06-02/derived/visit.dates.FACHILD', c = T)  %>% 
  mutate(avisit = factor(avisit, levels = c("BL","V02","V03","V04","V05","V06","V07"))) %>% 
  select  (study, sjid, avisit, avisitn, adt, hpf, fpf, flagged) %>% 
  group_by(study, sjid, avisit, avisitn, adt,           flagged) %>%
  summarise_all( ~toString(unique(sort(na.omit(.)))))

dt. %>% filter(flagged == T)

# for visit stats, use best option ----------------------------------------

dt. %<>%
  group_by(sjid) %>% 
  mutate ( time.      = as.numeric(adt-min(adt))/365.25) %>% 
  mutate ( window.dev = abs( time. - avisitn ) ) %>% 
  group_by( study, sjid, avisitn ) %>% 
  filter ( window.dev == min(window.dev) )

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
  aes(y = n)+scale_y_continuous(breaks = c(0,25,50,75,100))+
  geom_text(aes(label = paste(n,'\n(',pct,'%)', sep = '')), nudge_y = -5, color = 'white')+
  theme_minimal(base_size = 20)+theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())+
  .leg_tr+
  xlab('Visit Time')+
  ylab('Number of Subjects')+
  coord_cartesian(ylim= c(0,105))

# .sp()

# In-Person Visits % -----------------------------------------------------------------

dt.tmp <- dt. %>% ungroup %>% 
  select(study, sjid, avisitn, hpf) %>% 
  unique() %>% 
  mutate(N   = length((sjid))) %>% 
  mutate(hpf = case_when(
    hpf == '1'                            ~ "in-person",
    hpf %in% c('1, 2', '1, 3', '1, 2, 3') ~ "mixed",
    hpf %in% c('2','3','2, 3', '1, 2, 3') ~ "virtual"
  )) %>%
  group_by( study, hpf, N ) %>%
  count() %>% 
  mutate(pct = round(100*n/(N), 0)) %>%
  # ungroup %>% select(hpf) %>% table
  mutate(hpf = factor(hpf, c('in-person','mixed','virtual')))


dt.tmp <- dt. %>% 
  group_by(avisit) %>% 
  mutate(N   = length(unique(sjid))) %>% 
  mutate(hpf = case_when(
    hpf == '1'                            ~ "in-person",
    hpf %in% c('1, 2', '1, 3', '1, 2, 3') ~ "mixed",
    hpf %in% c('2','3','2, 3', '1, 2, 3') ~ "virtual"
    )) %>%
  group_by( study, avisit, avisitn, N, hpf ) %>%
  count() %>% 
  mutate(pct = round(100*n/(N), 0)) %>%
  # ungroup %>% select(hpf) %>% table
  mutate(hpf = factor(hpf, c('in-person','mixed','virtual')))

dt.tmp %>% 
  ggplot()+geom_col(position = position_stack(reverse = TRUE))+
  aes(x = avisitn)+scale_x_continuous(breaks = c(0, .5, 1, 1.5, 2, 3, 4), labels = c('BL', '6M','1y','18M','2y','3y','3y+'))+
  aes(y = n)+scale_y_continuous(breaks = c(0,25,50,75,100))+
  aes(fill = hpf)+
  geom_text(aes(label = paste(n,'\n(',pct,'%)', sep = '')), nudge_y = -5, color = 'white', data = filter(dt.tmp, hpf == 'in-person'))+
  theme_minimal(base_size = 20)+theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())+
  .leg_tr+
  xlab('Visit Time')+
  ylab('Number of Subjects')+
  coord_cartesian(ylim = c(0,105))

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
  theme_minimal(base_size = 20)+theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())+
  .leg_tr+
  xlab('Visit Time')+
  ylab('Number of Subjects')+
  coord_cartesian(ylim = c(0,105))

# .sp()  

# FARS Visits -------------------------------------------------------------------

# dt. <- .dd.atx('visit.dates.FACHILD', c = T) %>% 
#   filter(crf == 'faneuro') %>% 
#   mutate(avisit = factor(avisit, levels = c("BL","V02","V03","V04","V05","V06","V07"))) %>% 
#   select  (study, sjid, avisit, avisitn, adt, hpf, fpf, flagged) %>% 
#   group_by(study, sjid, avisit, avisitn, adt,           flagged) %>%
#   summarise_all( ~toString(unique(sort(na.omit(.)))))

dt. <- .dd.atx('fars', c = T) %>% filter(paramcd == 'mFARS', study == 'FACHILD')

dt.tmp <- dt. %>% 
  # group_by(sjid) %>% 
  # mutate( N = n()) %>% 
  # mutate ( time.      = as.numeric(adt-min(adt))/365.25) %>% 
  # mutate ( window.dev = abs( time. - avisitn ) ) %>% 
  # group_by( study, sjid, avisitn ) %>% 
  # mutate ( dev.cat = cut(window.dev, c(0, 0.25, 0.5, 1, 10), labels = c('<3M', '>3M', '>6M', '>1y'), 
  #                        include.lowest = T), ) %>% 
  group_by( avisit ) %>% 
  mutate ( N   = length(unique(sjid))) %>% 
  group_by( study, avisit, avisitn, N ) %>%
  count() %>% 
  mutate(pct = round(100*n/(108), 0))

dt.tmp %>% 
  ggplot()+geom_col(position = position_stack(reverse = TRUE))+
  aes(x = avisitn)+scale_x_continuous(breaks = c(0, .5, 1, 1.5, 2, 3, 4), labels = c('BL', '6M','1y','18M','2y','3y','3y+'))+
  # aes(x = avisitn)+scale_x_continuous(breaks = c(0, .5, 1, 1.5, 2, 3, 4), labels = levels(dt.$avisit)[c(1:7)])+
  aes(y = n)+scale_y_continuous(breaks = seq(0, 100, 25))+
  # aes(fill = dev.cat)+scale_fill_brewer(palette = 'Set1', direction = -1)+
  # geom_text(aes(label = paste(n,'\n(',pct,'%)', sep = '')), nudge_y = -5, color = 'black', 
  #           data = filter(dt.tmp, dev.cat == '<3M'))+
  geom_text(aes(label = paste(n,'\n(',pct,'%)', sep = '')), nudge_y = -5, color = 'white')+
  theme_minimal(base_size = 20)+theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())+
  .leg_tr+
  xlab('Visit Time')+
  ylab('Number of Subjects')+
  coord_cartesian(ylim = c(0,105))

# .sp()

# FARS Visits Windows -----------------------------------------------------

dt.tmp <- dt. %>% 
  group_by(sjid) %>% 
  mutate ( time.      = as.numeric(adt-min(adt))/365.25) %>% 
  mutate ( window.dev = abs( time. - avisitn ) ) %>% 
  group_by( study, sjid, avisitn ) %>% 
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
  geom_text(aes(label = paste(n,'\n(',pct,'%)', sep = '')), nudge_y = -5, color = 'black',
            data = filter(dt.tmp, dev.cat == '<3M'))+
  theme_minimal(base_size = 20)+theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())+
  .leg_tr+
  xlab('Visit Time')+
  ylab('Number of Subjects')+
  coord_cartesian(ylim = c(0,105))

# .sp()  

