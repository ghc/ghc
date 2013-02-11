

testFile = 'nothing'
if config.platform.startswith('i386-'):
        testFile  = 'test-i386.dat'
        outFile   = 'result-i386.txt'

elif config.platform.startswith('x86_64-'):
        testFile  = 'test-x86_64.dat'
        outFile   = 'result-x86_64.txt'

elif config.platform.startswith('sparc-'):
        testFile  = 'test-sparc.dat'
        outFile   = 'result-sparc.txt'


if testFile != 'nothing':
 test   ('dph-smvm-copy'
        , [ outputdir('copy')
          , when(fast(), skip)
          , reqlib('dph-lifted-copy')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2'])
          , extra_run_opts(testFile + " " + outFile)
          ]
        , multimod_compile_and_run
        , [ 'Main'
          , '-Odph -package dph-lifted-copy -package dph-prim-par'])

 test   ('dph-smvm-vseg'
        , [ outputdir('vseg')
          , when(fast(), skip)
          , reqlib('dph-lifted-vseg')
          , reqlib('dph-prim-par')
          , only_ways(['normal', 'threaded1', 'threaded2'])
          , extra_run_opts(testFile + " " + outFile)
          ]
        , multimod_compile_and_run
        , [ 'Main'
          , '-Odph -package dph-lifted-vseg -package dph-prim-par'])

