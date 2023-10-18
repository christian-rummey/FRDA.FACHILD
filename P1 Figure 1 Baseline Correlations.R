
# data --------------------------------------------------------------------

rm(list=ls())
.mycols <- c('#e41a1c','#457EB7','black')

dt. <- readRDS('DATA derived/dt.rds')
dm. <- readRDS('DATA derived/dm.rds')
source('project.settings.R')

dt.tmp <- dt. %>%
  ungroup %>% 
  mutate ( param  = as.character( paramcd ) ) %>% 
  mutate ( param  = factor ( param, levels = pars., labels = params. )) %>% 
  droplevels() %>% 
  left_join(dm.) %>% 
  filter(itt) %>%
  filter(paramcd %in% c('mFARS','FARS.E','FARS.B','FARS.C')) %>% 
  filter(avisitn == 0) %>% 
  mutate( pmx = ifelse(pm %in% c('G130V','I154F'), T, F) )

# Figure 2 ----------------------------------------------------------------

dt.tmp %>%
  mutate(study = ifelse(pmx, 'pm', study)) %>% 
  filter(bl.amb == 'ambulatory') %>%
  # gather(paramcd, aval, FARS.Am, FARS.B, FARS.C, FARS.E, mFARS) %>% 
  # mutate_at('paramcd', factor, pars.) %>% 
  filter(paramcd != 'FARS.Am') %>%
  # filter(paramcd == 'FARS.B') %>% 
  ggplot()+geom_point()+
  aes(x = bl.age, y = aval)+
  aes(color = study)+scale_color_manual(values = .mycols)+
  aes(alpha = study)+scale_alpha_manual(values = c(1,0.5,1))+
  geom_smooth(method = lm, aes(group = study), se = F, data = filter(dt.tmp, !pmx ))+
  geom_smooth(method = lm, aes(group = study), se = F, data = filter(dt.tmp, !pmx ), linetype = 3)+
  facet_wrap(~param, scales = 'free_y')+
  labs(x = 'Age at Baseline', y = 'Score')+
  # geom_vline(xintercept = c(6,8,10), linetype = 2)+
  # geom_text(aes(label = pm), data = filter(dt.tmp, (pm %in% c('G130V','I154F')), paramcd == 'FARS.B'))+
  .leg_none+
  .box

# .sp(ti = 'Figure 2')

# . -----------------------------------------------------------------------

dt.tmp %>% 
  select(study, sjid, paramcd, aval, bl.age, pm, pmx) %>% 
  mutate(study = ifelse(pmx, 'pm', study)) %>% 
  spread(paramcd, aval) %>% 
  mutate(EB.ratio = FARS.E/FARS.B) %>% 
  filter(EB.ratio>10)
  ggplot()+geom_point()+
  aes(x = bl.age, y = EB.ratio)+
  aes(color = study)+scale_color_manual(values = .mycols)+
  aes(alpha = study)+scale_alpha_manual(values = c(1,0.5,1))+
  # geom_smooth(method = lm, aes(group = study), se = F, data = filter(dt.tmp, !pmx ))+
  # geom_smooth(method = lm, aes(group = study), se = F, data = filter(dt.tmp, !pmx ), linetype = 3)+
  # facet_wrap(~paramcd, scales = 'free_y')+
  # labs(x = 'Age at Baseline', y = 'Score')+
  # geom_vline(xintercept = c(6,8,10), linetype = 2)+
  # geom_text(aes(label = pm), data = filter(dt.tmp, (pm %in% c('G130V','I154F')), paramcd == 'FARS.B'))+
  .leg_none+
  .box


# dm. %>%
#   filter(itt) %>%
#   mutate(dur = bl.age-aoo) %>%
#   mutate(median.dur = median(dur, na.rm=T)) %>% filter(!is.na(dur)) %>%
#   mutate(med.dur = ifelse(dur>median.dur, 'long','short')) %>%
#   gather(paramcd, aval, FARS.B, FARS.C, FARS.E, mFARS) %>%
#   mutate_at('paramcd', factor, pars.) %>%
#   filter(paramcd == 'FARS.B') %>%
#   ggplot()+geom_point()+
#   aes(x = bl.age, y = aval)+
#   aes(shape = bl.amb)+.ssmA+
#   aes(color = study)+.scbs1+
#   aes(alpha = study)+scale_alpha_manual(values = c(1,0.5))+
#   geom_smooth(method = lm, aes(group = study), se = F)+
#   facet_wrap(~med.dur, scales = 'free_y')+
#   # labs(x = 'Age at Baseline', y = 'Score')+
#   guides(shape = 'none')
# # 
# # ggExtra::ggMarginal(last_plot(), groupColour = T)
# # # filter(dm., mFARS>79)
