if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('mhu-closure',
     extra_files(['p/', 'q/', 'r/', 'r1/', 'unitP', 'unitQ', 'unitR', 'unitR1', 'Setup.hs']),
     run_command,
     ['$MAKE -s --no-print-directory mhu-closure ' + cleanup])
