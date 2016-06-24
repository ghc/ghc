if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('cabal05',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory cabal05 ' + cleanup])
