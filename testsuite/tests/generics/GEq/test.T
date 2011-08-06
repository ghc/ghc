setTestOpts(only_compiler_types(['ghc']))

test('GEq1', extra_clean(['GEq1A.hi', 'GEq1A.o']),
     multimod_compile_and_run, ['GEq1', ''])
test('GEq2', normal, multimod_compile_and_run, ['GEq2', ''])