test('determ002',
     extra_clean(['A.o', 'A.hi', 'A.old.hi']),
     run_command,
     ['$MAKE -s --no-print-directory determ002'])
