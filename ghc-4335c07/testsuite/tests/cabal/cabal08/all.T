if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('cabal08',
     extra_files(['Main.hs', 'Setup.hs', 'p1/', 'p2/']),
     run_command,
     ['$MAKE -s --no-print-directory cabal08 ' + cleanup])
