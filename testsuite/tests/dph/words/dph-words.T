
test    ('dph-words-copy-opt' 
        , [ alone
          , skip_if_fast
          , extra_clean(['Main.hi', 'Main.o', 'WordsVect.hi', 'WordsVect.o'])
          , reqlib('dph-lifted-copy')
          , reqlib('dph-prim-par')
          , only_ways(['normal']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-Odph -package dph-lifted-copy -package dph-prim-par -fno-liberate-case -fno-spec-constr'])


test    ('dph-words-vseg-opt' 
        , [ alone
          , skip_if_fast
          , extra_clean(['Main.hi', 'Main.o', 'WordsVect.hi', 'WordsVect.o'])
          , reqlib('dph-lifted-vseg')
          , reqlib('dph-prim-par')
          , only_ways(['normal']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-Odph -package dph-lifted-vseg -package dph-prim-par -fno-liberate-case -fno-spec-constr'])


test    ('dph-words-copy-fast' 
        , [ extra_clean(['Main.hi', 'Main.o', 'WordsVect.hi', 'WordsVect.o'])
          , reqlib('dph-lifted-copy')
          , reqlib('dph-prim-par')
          , only_ways(['normal']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-O -fno-enable-rewrite-rules -fno-spec-constr -package dph-lifted-copy -package dph-prim-par'])


test    ('dph-words-vseg-fast' 
        , [ extra_clean(['Main.hi', 'Main.o', 'WordsVect.hi', 'WordsVect.o'])
          , reqlib('dph-lifted-vseg')
          , reqlib('dph-prim-par')
          , only_ways(['normal']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-O -fno-enable-rewrite-rules -fno-spec-constr -package dph-lifted-vseg -package dph-prim-par'])

