test('T14611',
     [extra_files(['foo.c', 'main.hs', 'foo_dll.c']),
      expect_broken(18718),
      unless(opsys('mingw32'), skip)],
     makefile_test, ['T14611'])
