if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('different-db',
     [ extra_files(['p/', 'q/', 'r/', 'p1/', 'unitP', 'unitQ', 'unitR', 'unitP1', 'Setup.hs'])
     , js_broken(22349)
     ],
     run_command,
     ['$MAKE -s --no-print-directory different-db ' + cleanup])
