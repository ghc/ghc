
test    ('dph-diophantine-copy-opt' 
        , [ alone
          , skip_if_fast
          , reqlib('dph-lifted-copy')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-Odph -package dph-lifted-copy'])

test    ('dph-diophantine-copy-fast' 
        , [ alone
          , reqlib('dph-lifted-copy')
          , reqlib('dph-prim-par')
          , extra_clean(['DiophantineVect.hi', 'DiophantineVect.o',
                         'Main.hi', 'Main.o'])
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-O -fno-enable-rewrite-rules -package dph-lifted-copy'])

