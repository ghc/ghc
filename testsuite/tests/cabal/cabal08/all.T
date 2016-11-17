if default_testopts.cleanup != '':
   cleanup = 'CLEANUP=1'
else:
   cleanup = ''

# This test is broken since we eliminated the fancy shadowing logic; skip it for
# now. See #12485.
test('cabal08',
     skip,
     run_command,
     ['$MAKE -s --no-print-directory cabal08 ' + cleanup])
