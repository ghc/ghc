
test('queens',
     [skip_if_fast,
      extra_clean(['Main.hi', 'Main.o'])],
     compile_and_run,
     [''])
