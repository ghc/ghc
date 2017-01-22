# Test for #12723, a recompilation bug

test('recomp016', [], run_command, ['$MAKE -s --no-print-directory recomp016'])
