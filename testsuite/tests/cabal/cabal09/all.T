if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('cabal09',
     [extra_files(['Main.hs', 'Setup.hs', 'reexport.cabal']),
      when(opsys('mingw32'), expect_broken(16386))],
     run_command,
     ['$MAKE -s --no-print-directory cabal09 ' + cleanup])
