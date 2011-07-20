setTestOpts(only_compiler_types(['ghc']))

test('GUniplate1', normal, multimod_compile_and_run, ['Main', ''])