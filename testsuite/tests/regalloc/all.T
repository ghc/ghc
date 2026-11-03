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

# A loop whose own return continuation started with R1 allocated, though R1
# is not live into it, so that its back edge paid fixup moves (joinRR and
# joinRM in -ddump-asm-stats, which must be 0; x86-64)
test('T27981',
     [ unless(arch('x86_64'), skip),
       when(not have_ncg(), skip),
       only_ways(['normal']),
       grep_errmsg(r'^\(\d+, \d+, \d+, (?!0, 0,)(\d+, \d+),', [1]) ],
     compile,
     ['-O -ddump-asm-stats'])

# A procedure holding a block the allocator has no estimated frequency for,
# as -finfo-table-map leaves the continuation of an unreachable call: its
# blocks were allocated in the given order, and the loop's back edge paid a
# fixup move for the registers its return continuation chose (#27800). Only
# the continuation's jump into the loop may pay one (joinRR at most 1 and
# joinRM 0 in -ddump-asm-stats; x86-64)
test('T27800_itm',
     [ cmm_src,
       unless(arch('x86_64'), skip),
       when(not have_ncg(), skip),
       only_ways(['optasm']),
       grep_errmsg(r'^\(\d+, \d+, \d+, (?![01], 0,)(\d+, \d+),', [1]) ],
     compile,
     ['-no-hs-main -finfo-table-map -ddump-asm-stats'])
