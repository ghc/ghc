if default_testopts.cleanup != '':
   cleanup = 'CLEANUP=1'
else:
   cleanup = ''

test('cabal08',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory cabal08 ' + cleanup])
