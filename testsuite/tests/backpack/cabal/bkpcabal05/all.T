if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('bkpcabal05',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory bkpcabal05 ' + cleanup])
