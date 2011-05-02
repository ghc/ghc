setTestOpts(only_compiler_types(['ghc']))

test('Uniplate1', normal, multimod_compile_and_run, ['Main', ''])