test    ('dph-classes-vseg-fast' 
        , [ expect_fail
          , extra_clean(['Main.o', 'Main.hi', 'DefsVect.hi', 'DefsVect.o'])
          , reqlib('dph-lifted-vseg')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-O -fno-enable-rewrite-rules -package dph-lifted-vseg'])
