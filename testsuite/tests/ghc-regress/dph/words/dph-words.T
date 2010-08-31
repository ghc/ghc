
test    ('dph-words-opt' 
        , [ alone
          , skip_if_fast
          , reqlib('dph-par')
          , reqlib('dph-prim-par')
          , only_ways(['normal']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-Odph -funbox-strict-fields -fcpr-off -package dph-par -package dph-prim-par'])


test    ('dph-words-fast' 
        , [ reqlib('dph-par')
          , reqlib('dph-prim-par')
          , only_ways(['normal']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-O -fno-rewrite-rules -package dph-par -package dph-prim-par'])


