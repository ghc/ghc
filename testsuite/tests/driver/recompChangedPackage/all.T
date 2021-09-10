if config.have_vanilla:
    vanilla = '--enable-library-vanilla'
else:
    vanilla = '--disable-library-vanilla'

if config.have_profiling:
    prof = '--enable-library-profiling'
else:
    prof = '--disable-library-profiling'

if not config.compiler_profiled and config.have_dynamic:
    dyn = '--enable-shared'
else:
    dyn = '--disable-shared'

test('recompChangedPackage', [extra_files(['q', 'Main.hs', 'Setup.hs', 'PLib1.hs']),
                   when(fast(), skip)],
     run_command,
     ['$MAKE -s --no-print-directory recompChangedPackage VANILLA=' + vanilla + ' PROF=' + prof + ' DYN=' + dyn])
