if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('sigcabal01',
     expect_broken(10622),
     run_command,
     ['$MAKE -s --no-print-directory sigcabal01 ' + cleanup])
