if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('mhu-closure',
     [ extra_files(['p/', 'q/', 'r/', 'r1/', 'unitP', 'unitQ', 'unitR', 'unitR1', 'Setup.hs'])
     , js_broken(22349)
     ],
     run_command,
     ['$MAKE -s --no-print-directory mhu-closure ' + cleanup])
