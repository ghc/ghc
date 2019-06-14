test('regalloc_unit_tests',
     [ when(unregisterised(), skip), extra_files(['no_spills.cmm']),
       when(not have_ncg(), skip),
       [ignore_stderr, only_ways(['normal'])], extra_run_opts('"' + config.libdir + '"') ],
     compile_and_run,
     ['-package ghc'])
