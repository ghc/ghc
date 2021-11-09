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

test('recompPluginPackage', [extra_files(['p', 'q', 'Setup.hs']),
                   when(opsys('mingw32'), fragile(16405)),
                   when(fast(), skip),
                   unless(have_dynamic(), skip)],
     run_command,
     ['$MAKE -s --no-print-directory recompPluginPackage VANILLA=' + vanilla + ' PROF=' + prof + ' DYN=' + dyn])
