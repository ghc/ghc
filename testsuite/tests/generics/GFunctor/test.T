setTestOpts(only_compiler_types(['ghc']))

test('GFunctor1', extra_clean(['GFunctor.hi', 'GFunctor.o', 'Main.hi', 'Main.o']),
     multimod_compile_and_run, ['Main', ''])