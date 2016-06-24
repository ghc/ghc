if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('cabal03',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory cabal03 ' + cleanup])
