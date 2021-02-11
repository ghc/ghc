if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('cabal03',
     extra_files(['Setup.lhs', 'p/', 'q/', 'r/']),
     run_command,
     ['$MAKE -s --no-print-directory cabal03 ' + cleanup])
