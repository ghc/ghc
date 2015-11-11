test('prog014',
   [ extra_run_opts('dummy.o')
   , pre_cmd('$MAKE -s --no-print-directory prog014')
   , clean_cmd('rm -f dummy.o')
   ],
   ghci_script,
   ['prog014.script'])

