test('T12031', [ extra_clean(['bar.o', 'baz.o', 'ExternBug.o']) ],
               run_command, ['$MAKE -s --no-print-directory T12031'])
