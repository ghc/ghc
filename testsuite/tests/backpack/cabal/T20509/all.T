if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('T20509',
     [extra_files(['p', 'q', 'T20509.cabal', 'Setup.hs']),
      run_timeout_multiplier(2)],
     run_command,
     ['$MAKE -s --no-print-directory T20509 ' + cleanup])
