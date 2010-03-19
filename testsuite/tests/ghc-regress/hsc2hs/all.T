
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

