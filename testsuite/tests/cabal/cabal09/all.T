if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('cabal09',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory cabal09 ' + cleanup])
