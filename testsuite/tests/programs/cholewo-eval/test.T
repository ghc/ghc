test('cholewo-eval',
     [skip_if_fast,
      extra_clean(['Main.hi', 'Main.o', 'Arr.hi', 'Arr.o'])],
     multimod_compile_and_run,
     ['Main', ''])
