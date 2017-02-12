if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

# Test recompilation checking on signatures
test('bkpcabal04',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory bkpcabal04 ' + cleanup])
