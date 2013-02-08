
test    ('dph-nbody-vseg-opt' 
        , [ high_memory_usage
          , outputdir('vseg-opt')
          , skip_if_fast
          , reqlib('dph-lifted-vseg')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-Odph -fno-liberate-case -package dph-lifted-vseg -package dph-prim-par'])


test    ('dph-nbody-copy-opt' 
        , [ high_memory_usage
          , outputdir('copy-opt')
          , skip_if_fast
          , reqlib('dph-lifted-copy')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-Odph -fno-vectorisation-avoidance -fno-liberate-case -package dph-lifted-copy -package dph-prim-par'])


test    ('dph-nbody-vseg-fast' 
        , [ outputdir('vseg-fast')
          , reqlib('dph-lifted-vseg')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-O0 -package dph-lifted-vseg -package dph-prim-par'])


test    ('dph-nbody-copy-fast' 
        , [ outputdir('copy-fast')
          , reqlib('dph-lifted-copy')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-O0 -fno-vectorisation-avoidance -package dph-lifted-copy -package dph-prim-par'])
