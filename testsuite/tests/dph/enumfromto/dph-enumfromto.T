test    ('EnumFromToP' 
        , [ extra_clean(['EnumFromToP.o', 'EnumFromToP.hi'])
          , reqlib('dph-lifted-vseg')
          , reqlib('dph-prim-par')
          , ignore_output
          , expect_broken(7736)
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , compile_fail
        , [ '-O -fno-enable-rewrite-rules -package dph-lifted-vseg'])
