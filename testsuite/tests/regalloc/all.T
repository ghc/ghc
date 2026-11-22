test('regalloc_unit_tests',
     [ when(unregisterised(), skip), extra_files(['no_spills.cmm']),
       when(not have_ncg(), skip),
       # no regalloc business on wasm32
       when(arch('wasm32'), expect_fail),
       [ignore_stderr, only_ways(['normal'])], extra_run_opts('"' + config.libdir + '"') ],
     compile_and_run,
     ['-package ghc'])

# #27981: Test for lack of a fixup block.
# The regex is a bit scary but it's basically:
# * Look for "f:"" as sanity check.
# * Look for "Ln<something>:" - The label generated for a fixup block.
#
# We only test x86 as the bug can't currently be triggered on the other backends.
test('T27981',
     [ when(not arch('x86_64'), skip),
       when(unregisterised(), skip),
       when(not have_ncg(), skip),
       only_ways(['normal']),
       cmm_src,
       grep_errmsg(r'^_?(f):|(Ln)\w*:', [1, 2]) ],
     compile, ['-O -ddump-asm'])
