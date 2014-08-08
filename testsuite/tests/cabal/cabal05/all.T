if default_testopts.cleanup != '':
   cleanup = 'CLEANUP=1'
else:
   cleanup = ''

test('cabal05',
     ignore_output,
     run_command,
     ['$MAKE -s --no-print-directory cabal05 ' + cleanup])
