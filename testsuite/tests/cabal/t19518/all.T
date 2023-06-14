if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('t19518',
     [ extra_files(['Setup.hs', 'p/', 'q/', 'r/'])
     , js_broken(22349)
     ],
     run_command,
     ['$MAKE -s --no-print-directory t19518 ' + cleanup])
