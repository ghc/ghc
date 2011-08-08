setTestOpts(only_compiler_types(['ghc']))

test('GUniplate1', extra_clean(['GUniplate.hi', 'GUniplate.o',
                                'Main.hi', 'Main.o']),
     multimod_compile_and_run, ['Main', ''])