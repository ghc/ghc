
test    ('dph-sumnats-copy' 
        , [ outputdir('copy')
          , reqlib('dph-lifted-copy')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-Odph -package dph-lifted-copy'])

test    ('dph-sumnats-vseg' 
        , [ outputdir('vseg')
          , reqlib('dph-lifted-vseg')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-Odph -package dph-lifted-vseg'])

