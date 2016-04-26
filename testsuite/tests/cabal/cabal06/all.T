if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('cabal06',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory cabal06 ' + cleanup])
