test('T12771', [ extra_clean(['libfoo.a', 'libfoo-1.dll', 'foo.o', 'main.o'])
               , extra_files(['foo.c', 'main.hs', 'foo_dll.c'])
               , unless(opsys('mingw32'), skip)
               ],
               run_command, ['$MAKE -s --no-print-directory T12771'])
