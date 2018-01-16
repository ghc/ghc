# Test for #5614, a recompilation bug

test('recomp010', [extra_files(['Main.hs', 'X1.hs', 'X2.hs'])], run_command, ['$MAKE -s --no-print-directory recomp010'])
