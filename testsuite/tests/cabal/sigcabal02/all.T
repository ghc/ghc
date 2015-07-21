if default_testopts.cleanup != '':
   cleanup = 'CLEANUP=1'
else:
   cleanup = ''

test('sigcabal02',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory sigcabal02 ' + cleanup])
