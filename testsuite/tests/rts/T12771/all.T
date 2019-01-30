test('T12771',
     [extra_files(['foo.c', 'main.hs', 'foo_dll.c']),
      unless(opsys('mingw32'), skip)],
     makefile_test, ['T12771'])
