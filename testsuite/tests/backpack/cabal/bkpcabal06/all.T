if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('bkpcabal06',
     [extra_files(['bkpcabal06.cabal', 'Setup.hs', 'sig', 'impl']),
      when(opsys('mingw32'), skip)],
     run_command,
     ['$MAKE -s --no-print-directory bkpcabal06 ' + cleanup])
