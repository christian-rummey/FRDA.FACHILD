
parlst. <- c('Timed up and Go (1/s)'          , 'tug.i',
             'One Minute Walk (m)'            , 'w1m',
             '6 Minute Walk (m)'              , 'w6m',
             'Timed 25 Foot Walk (m/s)'       , 'w25.i',
             '9 Hole Peg Test (1/min)'        , 'hpt.i',
             'Timed 25 Foot Walk (m/s) unable', 'w25.iu',
             '9 Hole Peg Test (1/min) unable' , 'hpt.iu',
             'Berg Balance Scale'             , 'bbs',
             'Activities of Daily Living'     , 'ADL',
             'mFARS'                          , 'mFARS',
             'Upright Stability (FARS.E)'     , 'FARS.E',
             'Upper Limbs (FARS.B)'           , 'FARS.B',
             'Lower Limbs (FARS.C)'           , 'FARS.C',
             'Bulbar (FARS.Am)'               , 'FARS.Am' )

params. <- parlst.[c(seq(1,25,2))]
pars.   <- parlst.[c(seq(2,26,2))]

rm(parlst.)