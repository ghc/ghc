
test    ('dph-quickhull-copy-fast' 
        , [ outputdir('copy-fast')
          , reqlib('dph-lifted-copy')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-O0 -fno-vectorisation-avoidance -package dph-lifted-copy -package dph-prim-par'])


test    ('dph-quickhull-vseg-fast' 
        , [ outputdir('vseg-fast')
          , reqlib('dph-lifted-vseg')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-O0 -package dph-lifted-vseg -package dph-prim-par'])

