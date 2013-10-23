test    ('EnumFromToP' 
        , [ extra_clean(['EnumFromToP.o', 'EnumFromToP.hi'])
          , reqlib('dph-lifted-vseg')
          , reqlib('dph-prim-par')
          , ignore_output
          , expect_broken(7736)
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , compile_fail
        , [ '-O0 -package dph-lifted-vseg'])
