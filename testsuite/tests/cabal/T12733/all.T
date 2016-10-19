if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('T12733',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T12733 ' + cleanup])
