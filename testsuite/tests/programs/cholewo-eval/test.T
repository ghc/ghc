test('cholewo-eval',
     [when(fast(), skip),
      extra_clean(['Main.hi', 'Main.o', 'Arr.hi', 'Arr.o'])],
     multimod_compile_and_run,
     ['Main', ''])
