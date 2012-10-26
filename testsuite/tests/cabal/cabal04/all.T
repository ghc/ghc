setTestOpts(only_compiler_types(['ghc']))

if config.have_vanilla:
    vanilla = '--enable-library-vanilla'
else:
    vanilla = '--disable-library-vanilla'

if config.have_profiling:
    prof = '--enable-library-profiling'
else:
    prof = '--disable-library-profiling'

if config.have_shared_libs:
    dyn = '--enable-shared'
else:
    dyn = '--disable-shared'

if default_testopts.cleanup != '':
   cleanup = 'CLEANUP=1'
else:
   cleanup = ''

test('cabal04',
     normal,
     run_command, 
     ['$MAKE -s --no-print-directory cabal04 VANILLA=' + vanilla + ' PROF=' + prof + ' DYN=' + dyn + ' ' + cleanup])
