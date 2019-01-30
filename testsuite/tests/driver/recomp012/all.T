# Test for #7215, a recompilation bug

test('recomp012', [], run_command, ['$MAKE -s --no-print-directory recomp012'])
