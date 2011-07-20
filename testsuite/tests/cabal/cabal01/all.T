setTestOpts(only_compiler_types(['ghc']))

if config.have_profiling:
    prof = '--enable-library-profiling'
else:
    prof = ''

if default_testopts.cleanup != '':
   cleanup = 'CLEANUP=1'
else:
   cleanup = ''

test('cabal01',
     normal,
     run_command, 
     ['$MAKE -s --no-print-directory cabal01 PROF=' + prof + ' ' + cleanup])
