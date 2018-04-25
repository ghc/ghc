# Test that we check consistency of local type family instances
# with imported ones

test('T13092', extra_files(['A.hs', 'B.hs', 'C.hs', 'Main.hs']),
     run_command, ['$MAKE -s --no-print-directory T13092'])
