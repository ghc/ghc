
test    ('dph-nbody-vseg-opt' 
        , [ alone
          , skip_if_fast
          , extra_clean(['Main.hi',      'Main.o',
                         'Body.hi',      'Body.o',
                         'Config.hi',    'Config.o',
                         'Dump.hi',      'Dump.o',
                         'Solver.hi',    'Solver.o',
                         'Util.hi',      'Util.o',
                         'World.hi',     'World.o',
                         'Types.hi',     'Types.o',
                         'Generate.hi',  'Generate.o',
                         'Randomish.hi', 'Randomish.o'])
          , reqlib('dph-lifted-vseg')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-Odph -fno-liberate-case -package dph-lifted-vseg -package dph-prim-par'])


test    ('dph-nbody-copy-opt' 
        , [ alone
          , skip_if_fast
          , extra_clean(['Main.hi',      'Main.o',
                         'Body.hi',      'Body.o',
                         'Config.hi',    'Config.o',
                         'Dump.hi',      'Dump.o',
                         'Solver.hi',    'Solver.o',
                         'Util.hi',      'Util.o',
                         'World.hi',     'World.o',
                         'Types.hi',     'Types.o',
                         'Generate.hi',  'Generate.o',
                         'Randomish.hi', 'Randomish.o'])
          , reqlib('dph-lifted-copy')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-Odph -fno-liberate-case -package dph-lifted-copy -package dph-prim-par'])


test    ('dph-nbody-vseg-fast' 
        , [ alone
          , extra_clean(['Main.hi',      'Main.o',
                         'Body.hi',      'Body.o',
                         'Config.hi',    'Config.o',
                         'Dump.hi',      'Dump.o',
                         'Solver.hi',    'Solver.o',
                         'Util.hi',      'Util.o',
                         'World.hi',     'World.o',
                         'Types.hi',     'Types.o',
                         'Generate.hi',  'Generate.o',
                         'Randomish.hi', 'Randomish.o'])
          , reqlib('dph-lifted-vseg')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-O0 -package dph-lifted-vseg -package dph-prim-par'])


test    ('dph-nbody-copy-fast' 
        , [ alone
          , extra_clean(['Main.hi',      'Main.o',
                         'Body.hi',      'Body.o',
                         'Config.hi',    'Config.o',
                         'Dump.hi',      'Dump.o',
                         'Solver.hi',    'Solver.o',
                         'Util.hi',      'Util.o',
                         'World.hi',     'World.o',
                         'Types.hi',     'Types.o',
                         'Generate.hi',  'Generate.o',
                         'Randomish.hi', 'Randomish.o'])
          , reqlib('dph-lifted-copy')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2']) ] 
        , multimod_compile_and_run 
        , [ 'Main'
          , '-O0 -package dph-lifted-copy -package dph-prim-par'])
