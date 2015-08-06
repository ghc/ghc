if default_testopts.cleanup != '':
   cleanup = 'CLEANUP=1'
else:
   cleanup = ''

test('sigcabal01',
     expect_broken(10622),
     run_command,
     ['$MAKE -s --no-print-directory sigcabal01 ' + cleanup])
