
test    ('dph-words-opt' 
        , [ alone
          , skip_if_fast
          , extra_clean(['Main.hi', 'Main.o', 'WordsVect.hi', 'WordsVect.o'])
          , reqlib('dph-lifted-copy')
          , reqlib('dph-prim-par')
          , only_ways(['normal']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-Odph -package dph-lifted-copy -package dph-prim-par -fno-liberate-case'])


test    ('dph-words-fast' 
        , [ extra_clean(['Main.hi', 'Main.o', 'WordsVect.hi', 'WordsVect.o'])
          , reqlib('dph-lifted-copy')
          , reqlib('dph-prim-par')
          , only_ways(['normal']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-O -fno-enable-rewrite-rules -package dph-lifted-copy -package dph-prim-par'])

