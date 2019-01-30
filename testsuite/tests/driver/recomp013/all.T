# Test for #8247, a recompilation bug

test('recomp013', [], run_command, ['$MAKE -s --no-print-directory recomp013'])
