setTestOpts(only_compiler_types(['ghc']))

if config.have_profiling:
    prof = '--enable-library-profiling'
else:
    prof = ''

if config.have_shared_libs:
    dyn = '--enable-shared'
else:
    dyn = ''

if default_testopts.cleanup != '':
   cleanup = 'CLEANUP=1'
else:
   cleanup = ''

test('cabal04',
     normal,
     run_command, 
     ['$MAKE -s --no-print-directory cabal04 PROF=' + prof + ' DYN=' + dyn + ' ' + cleanup])
