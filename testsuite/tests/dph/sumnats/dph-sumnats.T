
test    ('dph-sumnats' 
        , [ extra_clean(['Main.hi', 'Main.o',
                         'SumNatsVect.hi', 'SumNatsVect.o'])
          , reqlib('dph-par')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-Odph -fdph-par'])


