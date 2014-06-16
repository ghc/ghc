setTestOpts(only_compiler_types(['ghc']))

test('cvh_unboxing',
     [when(fast(), skip),
      extra_clean(['Append.hi', 'Append.o',
                   'Main.hi',   'Main.o',
                   'Types.hi',  'Types.o'])],
     multimod_compile_and_run,
     ['Main', ''])

