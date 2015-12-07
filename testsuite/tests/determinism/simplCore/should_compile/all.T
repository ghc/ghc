test('determ006',
     extra_clean(['spec-inline-determ.o', 'spec-inline-determ.hi', 'spec-inline-determ.normal.hi']),
     run_command,
     ['$MAKE -s --no-print-directory determ006'])
