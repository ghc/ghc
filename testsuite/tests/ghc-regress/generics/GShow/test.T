setTestOpts(only_compiler_types(['ghc']))

test('GShow1', expect_broken(5227), multimod_compile_and_run, ['Main', ''])