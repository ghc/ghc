
test    ('dph-primespj-opt' 
        , [ alone
          , skip_if_fast
          , extra_clean(['Main.hi', 'Main.o', 'PrimesVect.hi', 'PrimesVect.o'])
          , reqlib('dph-par')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-Odph -package dph-par'])

test    ('dph-primespj-fast' 
        , [ extra_clean(['Main.hi', 'Main.o', 'PrimesVect.hi', 'PrimesVect.o'])
          , reqlib('dph-par')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-O -fno-enable-rewrite-rules -package dph-par'])

