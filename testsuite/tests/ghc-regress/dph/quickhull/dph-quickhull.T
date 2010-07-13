
test    ('dph-quickhull' 
        , [ reqlib('dph-par')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-Odph -funfolding-use-threshold30 -funbox-strict-fields -fcpr-off -package dph-par -package dph-prim-par'])


