if config.have_vanilla:
    vanilla = '--enable-library-vanilla'
else:
    vanilla = '--disable-library-vanilla'

if config.have_profiling:
    prof = '--enable-library-profiling'
else:
    prof = '--disable-library-profiling'

if not config.compiler_profiled and config.have_shared_libs:
    dyn = '--enable-shared'
else:
    dyn = '--disable-shared'

if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('cabal04',
     extra_files(['Library.hs', 'Setup.lhs', 'TH.hs', 'thtest.cabal']),
     run_command,
     ['$MAKE -s --no-print-directory cabal04 VANILLA=' + vanilla + ' PROF=' + prof + ' DYN=' + dyn + ' ' + cleanup])
