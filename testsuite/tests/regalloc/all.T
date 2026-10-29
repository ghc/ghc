test('regalloc_unit_tests',
     [ when(unregisterised(), skip), extra_files(['no_spills.cmm']),
       when(not have_ncg(), skip),
       # no regalloc business on wasm32
       when(arch('wasm32'), expect_fail),
       [ignore_stderr, only_ways(['normal'])], extra_run_opts('"' + config.libdir + '"') ],
     compile_and_run,
     ['-package ghc'])

# The liveness fixpoint must iterate until the register formats converge, not
# just the sets of live registers (#27619). A vector register live at
# VecFormat 2 FmtDouble around the loop must not be recorded at FF64.
test('T27619',
     [ unless(arch('x86_64'), skip),
       when(not have_ncg(), skip),
       only_ways(['normal']),
       grep_errmsg(r'%vV128_\S+ :: FF64') ],
     compile,
     ['-O -fno-cse -ddump-asm-liveness'])
