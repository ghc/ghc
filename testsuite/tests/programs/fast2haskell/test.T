
test('fast2haskell',
     [when(fast(), skip),
      extra_clean(['Main.hi',         'Main.o',
                   'Fast2haskell.hi', 'Fast2haskell.o'])],
     multimod_compile_and_run,
     ['Main', ''])

