test('regalloc_unit_tests',
     [ when(unregisterised(), skip), extra_files(['no_spills.cmm']),
       when(not have_ncg(), skip),
       # no regalloc business on wasm32
       when(arch('wasm32'), expect_fail),
       [ignore_stderr, only_ways(['normal'])], extra_run_opts('"' + config.libdir + '"') ],
     compile_and_run,
     ['-package ghc'])
