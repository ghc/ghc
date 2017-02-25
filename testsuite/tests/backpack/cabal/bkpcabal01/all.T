if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('bkpcabal01',
     extra_files(['p', 'q', 'impl', 'bkpcabal01.cabal', 'Setup.hs', 'Main.hs']),
     run_command,
     ['$MAKE -s --no-print-directory bkpcabal01 ' + cleanup])
