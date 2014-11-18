if default_testopts.cleanup != '':
   cleanup = 'CLEANUP=1'
else:
   cleanup = ''

test('sigcabal01',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory sigcabal01 ' + cleanup])
