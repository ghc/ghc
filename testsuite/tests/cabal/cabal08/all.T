if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('cabal08',
     expect_broken(13313),
     run_command,
     ['$MAKE -s --no-print-directory cabal08 ' + cleanup])
