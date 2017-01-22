# Test for #5614, a recompilation bug

test('recomp010', [], run_command, ['$MAKE -s --no-print-directory recomp010'])
