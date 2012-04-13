
test    ('dph-quickhull-copy-opt' 
        , [ alone
          , skip_if_fast
          , extra_clean(['Main.hi', 'Main.o',
                         'QuickHullVect.hi', 'QuickHullVect.o',
                         'SVG.hi', 'SVG.o',
                         'TestData.hi', 'TestData.o',
                         'Types.hi', 'Types.o'])
          , reqlib('dph-lifted-copy')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-Odph -funfolding-use-threshold30 -package dph-lifted-copy -package dph-prim-par'])


test    ('dph-quickhull-vseg-opt' 
        , [ alone
          , skip_if_fast
          , extra_clean(['Main.hi', 'Main.o',
                         'QuickHullVect.hi', 'QuickHullVect.o',
                         'SVG.hi', 'SVG.o',
                         'TestData.hi', 'TestData.o',
                         'Types.hi', 'Types.o'])
          , reqlib('dph-lifted-vseg')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-Odph -package dph-lifted-vseg -package dph-prim-par'])


test    ('dph-quickhull-copy-fast' 
        , [ alone
          , extra_clean(['Main.hi', 'Main.o',
                         'QuickHullVect.hi', 'QuickHullVect.o',
                         'SVG.hi', 'SVG.o',
                         'TestData.hi', 'TestData.o',
                         'Types.hi', 'Types.o'])
          , reqlib('dph-lifted-copy')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-O0 -package dph-lifted-copy -package dph-prim-par'])


test    ('dph-quickhull-vseg-fast' 
        , [ alone
          , extra_clean(['Main.hi', 'Main.o',
                         'QuickHullVect.hi', 'QuickHullVect.o',
                         'SVG.hi', 'SVG.o',
                         'TestData.hi', 'TestData.o',
                         'Types.hi', 'Types.o'])
          , reqlib('dph-lifted-vseg')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-O0 -package dph-lifted-vseg -package dph-prim-par'])

