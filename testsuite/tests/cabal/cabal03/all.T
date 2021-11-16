if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('cabal03',
     [javascript_failures, extra_files(['Setup.lhs', 'p/', 'q/', 'r/'])],
     run_command,
     ['$MAKE -s --no-print-directory cabal03 ' + cleanup])
