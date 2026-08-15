test('regalloc_unit_tests',
     [ when(unregisterised(), skip), extra_files(['no_spills.cmm']),
       when(not have_ncg(), skip),
       # no regalloc business on wasm32
       when(arch('wasm32'), expect_fail),
       [ignore_stderr, only_ways(['normal'])], extra_run_opts('"' + config.libdir + '"') ],
     compile_and_run,
     ['-package ghc'])

# Passes iff no vector vreg is recorded live at FF64. See T27619.hs.
test('T27619',
     [ unless(arch('x86_64'), skip),
       when(not have_ncg(), skip),
       only_ways(['normal']),
       grep_errmsg(r'%vV128_\S+ :: FF64') ],
     compile,
     ['-O -ddump-asm-liveness'])
