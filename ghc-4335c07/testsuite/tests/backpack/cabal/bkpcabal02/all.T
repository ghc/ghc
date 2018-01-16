if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('bkpcabal02',
     extra_files(['p', 'q', 'bkpcabal02.cabal', 'Setup.hs']),
     run_command,
     ['$MAKE -s --no-print-directory bkpcabal02 ' + cleanup])
