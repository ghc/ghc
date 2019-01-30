# Test for #481, a recompilation bug with Template Haskell

test('recomp009', [extra_files(['Main.hs', 'Sub1.hs', 'Sub2.hs'])], run_command, ['$MAKE -s --no-print-directory recomp009'])
