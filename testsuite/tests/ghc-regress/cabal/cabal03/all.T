if default_testopts.cleanup != '':
   cleanup = 'CLEANUP=1'
else:
   cleanup = ''

test('cabal03',
     ignore_output,
     run_command,
     ['$MAKE -s --no-print-directory cabal03 ' + cleanup])
