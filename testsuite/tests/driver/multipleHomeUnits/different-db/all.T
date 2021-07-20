if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('different-db',
     extra_files(['p/', 'q/', 'r/', 'p1/', 'unitP', 'unitQ', 'unitR', 'unitP1', 'Setup.hs']),
     run_command,
     ['$MAKE -s --no-print-directory different-db ' + cleanup])
