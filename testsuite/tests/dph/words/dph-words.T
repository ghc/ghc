
test    ('dph-words-copy-opt' 
        , [ high_memory_usage
          , outputdir('copy-opt')
          , when(fast(), skip)
          , reqlib('dph-lifted-copy')
          , reqlib('dph-prim-par')
          , only_ways(['normal']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-Odph -package dph-lifted-copy -package dph-prim-par -fno-liberate-case -fno-spec-constr'])


test    ('dph-words-vseg-opt' 
        , [ high_memory_usage
          , outputdir('vseg-opt')
          , when(fast(), skip)
          , reqlib('dph-lifted-vseg')
          , reqlib('dph-prim-par')
          , only_ways(['normal']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-Odph -package dph-lifted-vseg -package dph-prim-par -fno-liberate-case -fno-spec-constr'])


test    ('dph-words-copy-fast' 
        , [ outputdir('copy-fast')
          , reqlib('dph-lifted-copy')
          , reqlib('dph-prim-par')
          , only_ways(['normal']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-O0 -package dph-lifted-copy -package dph-prim-par'])


test    ('dph-words-vseg-fast' 
        , [ outputdir('vseg-fast')
          , reqlib('dph-lifted-vseg')
          , reqlib('dph-prim-par')
          , only_ways(['normal']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-O0 -package dph-lifted-vseg -package dph-prim-par'])

