setTestOpts(only_compiler_types(['ghc']))

test('GShow1', extra_clean(['GShow.hi', 'GShow.o', 'Main.hi', 'Main.o']),
     multimod_compile_and_run, ['Main', ''])