
test('hsc2hs001',
     [extra_clean(['hsc2hs001.hs', 'hsc2hs001_hsc_make.c']),
      if_platform('i386-unknown-mingw32', expect_broken(3929))],
     run_command,
     ['$MAKE -s --no-print-directory hsc2hs001'])

test('hsc2hs002',
     [extra_clean(['hsc2hs002.hs', 'hsc2hs002_hsc_make.c']),
      if_platform('i386-unknown-mingw32', expect_broken(3929))],
     run_command,
     ['$MAKE -s --no-print-directory hsc2hs002'])

test('hsc2hs003',
     [extra_clean(['hsc2hs003.hs', 'hsc2hs003_hsc_make.c']),
      if_platform('i386-unknown-mingw32', expect_broken(3929))],
     run_command,
     ['$MAKE -s --no-print-directory hsc2hs003'])

test('hsc2hs004',
     [extra_clean(['hsc2hs004.hs', 'hsc2hs004_hsc_make.c']),
      if_platform('i386-unknown-mingw32', expect_broken(3929))],
     run_command,
     ['$MAKE -s --no-print-directory hsc2hs004'])


test('3837',
     [extra_clean(['3837.hs', '3837_hsc_make.c']),
      if_platform('i386-unknown-mingw32', expect_broken(3929))],
     run_command,
     ['$MAKE -s --no-print-directory 3837'])

