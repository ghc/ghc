setTestOpts(only_compiler_types(['ghc']))

test('GMap1', extra_clean(['GMap.hi', 'GMap.o', 'Main.hi', 'Main.o']),
     multimod_compile_and_run, ['Main', ''])
