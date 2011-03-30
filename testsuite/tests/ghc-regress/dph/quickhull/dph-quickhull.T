
test    ('dph-quickhull-opt' 
        , [ alone
          , skip_if_fast
          , reqlib('dph-par')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-Odph -funfolding-use-threshold30 -fdph-par'])

test    ('dph-quickhull-fast' 
        , [ reqlib('dph-par')
          , reqlib('dph-prim-par')
          , expect_broken(5065)
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-O -fno-enable-rewrite-rules -fdph-par'])


