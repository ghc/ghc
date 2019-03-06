if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('cabal09',
     extra_files(['Main.hs', 'Setup.hs', 'reexport.cabal']),
     run_command,
     ['$MAKE -s --no-print-directory cabal09 ' + cleanup])
