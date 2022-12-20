
parlst. <- c('mFARS'                          , 'mFARS',
             'Upright Stability (FARS.E)'     , 'FARS.E',
             'Upper Limbs (FARS.B)'           , 'FARS.B',
             'Lower Limbs (FARS.C)'           , 'FARS.C',
             'Bulbar (FARS.Am)'               , 'FARS.Am',
             'Timed 25 Foot Walk (m/s)'       , 'w25.i',
             '9 Hole Peg Test (1/min)'        , 'hpt.i',
             # 'Timed 25 Foot Walk (m/s) unable', 'w25.iu',
             # '9 Hole Peg Test (1/min) unable' , 'hpt.iu',
             'Timed up and Go (1/s)'          , 'tug.i',
             'One Minute Walk (m)'            , 'w1m',
             '6 Minute Walk (m)'              , 'w6m',
             'Berg Balance Scale'             , 'bbs',
             'Activities of Daily Living'     , 'ADL',
             'PQL Total Score C'              , 'TOT.C',
             'PQL Total Score P'              , 'TOT.P',
             'PQL Physical C'                 , 'PF.C',
             'PQL Physical P'                 , 'PF.P',
             'PQL Emotional C'                , 'EF.C',
             'PQL Emotional P'                , 'EF.P',
             'PQL Social C'                   , 'SO.C',
             'PQL Social P'                   , 'SO.P',
             'PQL School C'                   , 'SC.C',
             'PQL School P'                   , 'SC.P'
             )

params. <- parlst.[c(seq(1,47,2))]
pars.   <- parlst.[c(seq(2,48,2))]

rm(parlst.)
