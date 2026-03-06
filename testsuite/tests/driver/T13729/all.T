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

test('T13729', [extra_files(['A', 'B', 'Setup.hs']),
                when(fast(), skip),
                js_broken(22349)],
     run_command,
     ['$MAKE -s --no-print-directory T13729 VANILLA=' + vanilla + ' PROF=' + prof + ' DYN=' + dyn])
