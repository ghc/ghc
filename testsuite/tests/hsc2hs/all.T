
test('hsc2hs001',
     extra_clean(['hsc2hs001.hs', 'hsc2hs001_hsc_make.c']),
     run_command,
     ['$MAKE -s --no-print-directory hsc2hs001'])

test('hsc2hs002',
     extra_clean(['hsc2hs002.hs', 'hsc2hs002_hsc_make.c']),
     run_command,
     ['$MAKE -s --no-print-directory hsc2hs002'])

test('hsc2hs003',
     extra_clean(['hsc2hs003.hs', 'hsc2hs003_hsc_make.c']),
     run_command,
     ['$MAKE -s --no-print-directory hsc2hs003'])

test('hsc2hs004',
     extra_clean(['hsc2hs004.hs', 'hsc2hs004_hsc_make.c']),
     run_command,
     ['$MAKE -s --no-print-directory hsc2hs004'])


test('T3837',
     [extra_clean(['T3837.hs', 'T3837_hsc_make.c'])],
     run_command,
     ['$MAKE -s --no-print-directory T3837'])

test('T4340',
     [extra_clean(['T4340.hs', 'T4340_hsc_make.c',
                   'T4340_hsc.c', 'T4340_hsc.h'])],
     run_command,
     ['$MAKE -s --no-print-directory T4340'])

test('T10272',
     [extra_clean(['T10272.hs', 'T10272_hsc_make.c'])],
     run_command,
     ['$MAKE -s --no-print-directory T10272'])

test('T12504',
     [extra_clean(['T12504/path/to/T12504.hs']),
      extra_files(['T12504']),
      ignore_stdout],
     run_command,
     ['$MAKE -s --no-print-directory T12504'])
