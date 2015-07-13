test('GUniplate1', extra_clean(['GUniplate.hi', 'GUniplate.o',
                                'Main.hi', 'Main.o']),
     multimod_compile_and_run, ['Main', ''])
