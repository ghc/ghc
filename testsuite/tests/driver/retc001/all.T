test('retc001',
     [clean_cmd('$MAKE -s clean'),
# See issue 11204, this test sometimes passes and sometimes fails on OSX
      when(opsys('darwin'), skip)],
     run_command,
     ['$MAKE -s --no-print-directory retc001'])

