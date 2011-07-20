setTestOpts(only_compiler_types(['ghc']))

test('GEq1', normal, multimod_compile_and_run, ['Main', ''])
test('GEq2', normal, multimod_compile_and_run, ['GEq2', ''])