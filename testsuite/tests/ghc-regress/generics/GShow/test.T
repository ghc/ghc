setTestOpts(only_compiler_types(['ghc']))

test('GShow1', normal, multimod_compile_and_run, ['Main', ''])