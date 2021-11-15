if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('cabal05',
     [when(opsys('OSJavaScript'), skip),
      extra_files(['Setup.hs', 'p/', 'q/', 'r/', 's/', 't/'])],
     run_command,
     ['$MAKE -s --no-print-directory cabal05 ' + cleanup])
