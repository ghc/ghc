if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('T15594',
     extra_files(['Setup.hs', 'Stuff.hs', 'Sig.hsig', 'pkg.cabal', 'src']),
     run_command,
     ['$MAKE -s --no-print-directory T15594 ' + cleanup])
