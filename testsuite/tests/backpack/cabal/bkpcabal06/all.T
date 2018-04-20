if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('bkpcabal06',
     [extra_files(['bkpcabal06.cabal', 'Setup.hs', 'sig', 'impl']),
      expect_broken(14759)],
     run_command,
     ['$MAKE -s --no-print-directory bkpcabal06 ' + cleanup])
