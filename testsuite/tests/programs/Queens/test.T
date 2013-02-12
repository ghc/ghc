
test('queens',
     [when(fast(), skip),
      extra_clean(['Main.hi', 'Main.o'])],
     compile_and_run,
     [''])
