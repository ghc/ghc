if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('T12733',
     [extra_files(['p/', 'q/', 'Setup.hs'])
     , js_broken(22349)
     ],
     run_command,
     ['$MAKE -s --no-print-directory T12733 ' + cleanup])
