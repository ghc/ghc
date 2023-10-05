test('T7289',
     [extra_files(['fp.c']), unless(opsys('mingw32'), skip),
      only_ways(['threaded1'])],
     compile_and_run, ['fp.c'])
