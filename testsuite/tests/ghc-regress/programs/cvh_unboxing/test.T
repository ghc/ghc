setTestOpts(only_compiler_types(['ghc']))

test('cvh_unboxing',
     [skip_if_fast,
      extra_clean(['Append.hi', 'Append.o',
                   'Main.hi',   'Main.o',
                   'Types.hi',  'Types.o'])],
     multimod_compile_and_run,
     ['Main', ''])

