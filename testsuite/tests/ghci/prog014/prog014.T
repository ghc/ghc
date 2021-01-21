test('prog014',
     [extra_files(['Primop.hs', 'dummy.c']),
      expect_fail, # bytecode compiler doesn't support foreign import prim
      extra_run_opts('dummy.o'),
      pre_cmd('$MAKE -s --no-print-directory prog014')],
     ghci_script, ['prog014.script'])
