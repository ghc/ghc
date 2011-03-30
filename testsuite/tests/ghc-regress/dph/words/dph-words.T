
test    ('dph-words-opt' 
        , [ alone
          , skip_if_fast
          , reqlib('dph-par')
          , reqlib('dph-prim-par')
          , only_ways(['normal']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-Odph -fdph-par'])


test    ('dph-words-fast' 
        , [ reqlib('dph-par')
          , reqlib('dph-prim-par')
          , expect_broken(5065)
          , only_ways(['normal']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-O -fno-enable-rewrite-rules -fdph-par'])


