test('T12031',
     [extra_files(['bar.c', 'baz.c', 'ExternBug.hs', 'foo.h']),
      unless(opsys('mingw32'), skip)],
     makefile_test, ['T12031'])
