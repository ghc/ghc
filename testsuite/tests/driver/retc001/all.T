test('retc001',
     [extra_files(['A.hs', 'B1.hs', 'B2.hs', 'C.hs']),
      clean_cmd('$MAKE -s clean'),
# See issue 11204, this test sometimes passes and sometimes fails on OSX
      when(opsys('darwin'), skip)],
     run_command,
     ['$MAKE -s --no-print-directory retc001'])

