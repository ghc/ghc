if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('bkpcabal05',
     [javascript_failures,
      extra_files(['bkpcabal05.cabal', 'A.hsig.in1', 'A.hsig.in2', 'M.hs', 'Setup.hs'])],
     run_command,
     ['$MAKE -s --no-print-directory bkpcabal05 ' + cleanup])
