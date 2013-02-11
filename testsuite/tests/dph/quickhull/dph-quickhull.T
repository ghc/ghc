
test    ('dph-quickhull-copy-opt' 
        , [ high_memory_usage
          , outputdir('copy-opt')
          , when(fast(), skip)
          , reqlib('dph-lifted-copy')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-Odph -fno-vectorisation-avoidance -funfolding-use-threshold30 -package dph-lifted-copy -package dph-prim-par'])


test    ('dph-quickhull-vseg-opt' 
        , [ high_memory_usage
          , outputdir('vseg-opt')
          , when(fast(), skip)
          , reqlib('dph-lifted-vseg')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-Odph -package dph-lifted-vseg -package dph-prim-par'])


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

