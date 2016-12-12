if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('bkpcabal03',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory bkpcabal03 ' + cleanup])
