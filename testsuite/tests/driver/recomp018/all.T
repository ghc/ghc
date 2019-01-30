# Test for regenerating .hie files that are out of date or missing

test('recomp018',
     [extra_files(['A.hs', 'B.hs', 'C.hs'])],
     run_command, ['$MAKE -s --no-print-directory recomp018'])
