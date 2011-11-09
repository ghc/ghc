
test    ('dph-words-opt' 
        , [ alone
          , skip_if_fast
          , extra_clean(['Main.hi', 'Main.o', 'WordsVect.hi', 'WordsVect.o'])
          , reqlib('dph-par')
          , reqlib('dph-prim-par')
          , only_ways(['normal']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-Odph -fdph-par -package dph-prim-par -fno-liberate-case'])


test    ('dph-words-fast' 
        , [ extra_clean(['Main.hi', 'Main.o', 'WordsVect.hi', 'WordsVect.o'])
          , reqlib('dph-par')
          , reqlib('dph-prim-par')
          , only_ways(['normal']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-O -fno-enable-rewrite-rules -fdph-par -package dph-prim-par'])


