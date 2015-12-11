test('retc001',
     [clean_cmd('$MAKE -s clean'),
      when(opsys('darwin'), expect_broken(11204))],
     run_command,
     ['$MAKE -s --no-print-directory retc001'])

