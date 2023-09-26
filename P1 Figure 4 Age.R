
# setup ---------------------------------------------------------------

rm(list=ls())
source('project.settings.R')

# data --------------------------------------------------------------------

parids  <- c(1,2,3,4)  # "mFARS"  "FARS.E" "FARS.B" "FARS.C"
# parids <- c(6,8,9,10) # "w25.i"  "tug.i"   "w1m"     "w6m"
# parids <- c(13,14,15,16) # PedsQL Total and Physical

pars.   <- pars.[c(parids)] 

est.aval <- readRDS('DATA derived/all estimates.rds') %>% 
  mutate ( param = as.character(param)) %>% 
  mutate ( param  = ifelse ( param == 'mFARS', 'mFARS Total', param )) %>% 
  mutate ( param  = factor(param, levels = params.)) %>% 
  filter ( paramcd %in% pars.) %>% 
  filter ( pop == 'age' ) %>% 
  droplevels()

# plot --------------------------------------------------------------------

.width = 0
.dodge = .075

clr <- 'black'
clr.errbars <- 'darkgrey'
clr <- NA
    
A <- est.aval %>%
  filter(paramcd %in% c('mFARS','FARS.E')) %>% 
  arrange(paramcd, avisit) %>% 
  ggplot()+
  geom_errorbar(width = .width, position = position_dodge(width = .dodge), color = clr.errbars)+
  aes(ymin = conf.low, ymax = conf.high)+
  ggh4x::geom_pointpath( position = position_dodge(width = .dodge) )+
  aes(linetype = study)+
  aes(group = paste(study, paramcd))+
  aes(x = avisitn , y = estimate)+scale_x_continuous(breaks = unique(est.aval$avisitn), labels = levels(est.aval$avisit))+
  aes(shape = study)+.ssmA+
  aes(color = value)+scale_color_manual(values = c('#e41a1c','#457EB7','#e41a1c','#457EB7'))+
  aes(group = paste(study, value))+
  facet_wrap(~param, scales = 'free', ncol = 2)+
  geom_hline(yintercept = 0, linetype = 2)+
  .leg_none+
  xlab('Visit')+
  ylab('')

B <- est.aval %>%
  filter((paramcd %in% c('FARS.B','FARS.C'))) %>%
  arrange(paramcd, avisit) %>% 
  ggplot()+
  geom_errorbar(width = .width, position = position_dodge(width = .dodge), color = clr.errbars)+
  aes(ymin = conf.low, ymax = conf.high)+
  ggh4x::geom_pointpath( position = position_dodge(width = .dodge) )+
  aes(linetype = study)+
  aes(group = paste(study, paramcd))+
  aes(x = avisitn , y = estimate)+scale_x_continuous(breaks = unique(est.aval$avisitn), labels = levels(est.aval$avisit))+
  aes(shape = study)+.ssmA+
  aes(color = value)+scale_color_manual(values = c('#e41a1c','#457EB7','#e41a1c','#457EB7'))+
  aes(group = paste(study, value))+
  facet_wrap(~param, scales = 'free', ncol = 2)+
  geom_hline(yintercept = 0, linetype = 2)+
  .leg_none+
  xlab('Visit')+
  ylab('Estimated Change from Baseline (95%CI)')


# ggpubr::ggarrange(A + coord_cartesian( ylim = c(-1  , 8)),
#                   B + coord_cartesian( ylim = c(-1.5, 4)),
#                   nrow = 2
# )
ggpubr::ggarrange(A + scale_y_continuous(breaks = c(0,4,8)) + coord_cartesian( ylim = c(-1  , 10)),
                  B + scale_y_continuous(breaks = c(0,2,4)) + coord_cartesian( ylim = c(-1.5, 4)),
                  nrow = 2
)

# .sp( ti = 'Figure 4, Age' )

