if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

# Test recompilation checking on signatures
test('bkpcabal04',
     extra_files(['p', 'q', 'bkpcabal04.cabal.in1', 'bkpcabal04.cabal.in2', 'Setup.hs']),
     run_command,
     ['$MAKE -s --no-print-directory bkpcabal04 ' + cleanup])
