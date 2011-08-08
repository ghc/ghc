
test    ('dph-dotp-opt' 
        , [ alone
          , skip_if_fast
          , extra_clean(['Main.o', 'Main.hi', 'DotPVect.hi', 'DotPVect.o'])
          , reqlib('dph-par')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-Odph -fdph-par'])

test    ('dph-dotp-fast' 
        , [ extra_clean(['Main.o', 'Main.hi', 'DotPVect.hi', 'DotPVect.o'])
          , reqlib('dph-par')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-O -fno-enable-rewrite-rules -fdph-par'])


