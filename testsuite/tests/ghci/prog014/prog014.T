test('prog014',
     [extra_files(['Primop.hs', 'dummy.c']),
      skip, # ghcjs
      extra_run_opts('dummy.o'),
      pre_cmd('$MAKE -s --no-print-directory prog014')],
     ghci_script, ['prog014.script'])
