test('T10672_x64',
     [extra_files(['Main.hs', 'Printf.hs', 'cxxy.cpp']),
      unless(opsys('mingw32'), skip), unless(arch('x86_64'), skip),
      when(opsys('mingw32'), expect_broken(16390))],
     makefile_test, ['T10672_x64'])
