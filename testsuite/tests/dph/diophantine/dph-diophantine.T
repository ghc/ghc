
test    ('dph-diophantine-opt' 
        , [ alone
          , skip_if_fast
          , reqlib('dph-lifted-copy')
          , reqlib('dph-prim-par')
          , expect_broken(5065)
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-Odph -package dph-lifted-copy'])

test    ('dph-diophantine-fast' 
        , [ reqlib('dph-lifted-copy')
          , reqlib('dph-prim-par')
          , extra_clean(['DiophantineVect.hi', 'DiophantineVect.o',
                         'Main.hi', 'Main.o'])
          , expect_broken(5613)
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-O -fno-enable-rewrite-rules -package dph-lifted-copy'])

