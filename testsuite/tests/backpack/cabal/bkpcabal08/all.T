if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('bkpcabal08',
     [extra_files(['p', 'q', 'impl', 'bkpcabal08.cabal', 'Setup.hs', 'R.hs']),
      js_broken(22351), 
      normalise_slashes,
      normalise_version('bkpcabal08')],
     run_command,
     ['$MAKE -s --no-print-directory bkpcabal08 ' + cleanup])
