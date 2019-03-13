if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('cabal10',
     extra_files(['Use.hs', 'Setup.hs', 'src/', 'internal-lib.cabal']),
     run_command,
     ['$MAKE -s --no-print-directory cabal10 ' + cleanup])
