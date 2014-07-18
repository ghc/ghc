if default_testopts.cleanup != '':
   cleanup = 'CLEANUP=1'
else:
   cleanup = ''

test('cabal06',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory cabal06 ' + cleanup])
