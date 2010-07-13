
test    ('dph-diophantine' 
        , [ alone
          , reqlib('dph-par')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-fcpr-off -package dph-par -package dph-prim-par'])


