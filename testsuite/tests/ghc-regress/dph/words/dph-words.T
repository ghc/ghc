
test    ('dph-words' 
        , [ reqlib('dph-par')
          , reqlib('dph-prim-par')
          , only_ways(['normal']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-Odph -funbox-strict-fields -fcpr-off -package dph-par -package dph-prim-par'])


