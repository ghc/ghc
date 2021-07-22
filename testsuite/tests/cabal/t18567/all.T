if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('T18567',
     extra_files(['Setup.hs', 'sublib/', 'sublib-unused', 'src/', 'internal-lib.cabal']),
     run_command,
     ['$MAKE -s --no-print-directory T18567 ' + cleanup])
