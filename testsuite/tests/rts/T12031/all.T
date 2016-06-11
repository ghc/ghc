test('T12031', [ extra_clean(['bar.o', 'baz.o', 'ExternBug.o'])
               , extra_files(['bar.c', 'baz.c', 'ExternBug.hs', 'foo.h'])
               ],
               run_command, ['$MAKE -s --no-print-directory T12031'])
