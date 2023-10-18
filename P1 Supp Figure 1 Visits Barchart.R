

# readRDS('DATA derived/visit.stats.rds') %>%
#   gather(par, value, FARS, other) %>% 
#   group_by(avisitn, par) %>% 
#   summarise(n=n()) %>% 
#   ggplot()+geom_col(position = position_dodge(width = 0))+
#   geom_text(aes(label = n))+
#   aes(x = avisitn, y = n)+
#   aes(fill = par)+
#   .leg_tr


# . -----------------------------------------------------------------------

# Overall Visits, for early Results Section
(6*108)
538/(6*108)


# dt. <- .dd.atx('fars', c = T) %>% filter(paramcd == 'mFARS', study == 'FACHILD')
dt. <- readRDS('DATA derived/visit.stats.rds')

dt.tmp <- dt. %>% 
  # group_by(sjid) %>% 
  # mutate( N = n()) %>% 
  # mutate ( time.      = as.numeric(adt-min(adt))/365.25) %>% 
  # mutate ( window.dev = abs( time. - avisitn ) ) %>% 
  # group_by( study, sjid, avisitn ) %>% 
  # mutate ( dev.cat = cut(window.dev, c(0, 0.25, 0.5, 1, 10), labels = c('<3M', '>3M', '>6M', '>1y'), 
  #                        include.lowest = T), ) %>% 
  group_by( study, avisitn, par ) %>% 
  # ungroup %>% select(study, avisitn, par) %>% table
  # mutate ( N   = length(unique(sjid))) %>% 
  summarise( n = length(unique(sjid))) %>% 
  spread(par, n) %>% 
  rename(all.visit = ALL) %>% 
  mutate(ALL.n = ifelse(study == 'FACHILD', 108, 289)) %>% 
  mutate(pct = round(100*FARS/(ALL.n), 0))

# all FACHILD visits
sum(filter(dt.tmp, study == 'FACHILD')$all.visit)

# all FACHILD mFARS visits
sum(filter(dt.tmp, study == 'FACHILD')$FARS)

dt.tmp %>% 
  filter(study == 'FACHILD') %>% 
  ggplot()+
  geom_col(position = position_stack(reverse = TRUE), fill = 'white', color = 'black')+
  geom_col(position = position_stack(reverse = TRUE), aes(y = FARS))+
  aes(x = avisitn)+scale_x_continuous(breaks = c(0, .5, 1, 1.5, 2, 3, 4), labels = c('BL', '6M','1y','18M','2y','3y','3y+'))+
  aes(y = all.visit)+scale_y_continuous(breaks = seq(0, 100, 25))+
  facet_wrap( ~study, scales ='free_y') +
  geom_text ( aes(y = FARS, label = paste(FARS,'\n(',pct,'%)', sep = '')), nudge_y = -5, color = 'white')+
  theme_minimal(base_size = 20)+theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())+
  .leg_tr+
  xlab('Visit')+
  ylab('Number of Subjects')
# +
#   coord_cartesian(ylim = c(0,105))



# withdrawals -------------------------------------------------------------

readRDS('../DATA/FACHILD/current/concl.rds') %>% 
  select(cno, patno, c_wddt, c_cnclstat, c_wdrsn5, c_wdrsncm, c_wddt) %>%
  filter(c_cnclstat != 1) %>% 
  select(c_wdrsn5) %>%
  table
  # arrange(c_wddt) %>% 
  # print(n=30)
  
  # 06 = Inability to continue giving consent
  # 07 = Unwilling/unable to commit time and/or resources, moved from area, etc.
  # 09 = Lost to Follow-up
  # 99 = Other

