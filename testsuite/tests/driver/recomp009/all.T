# Test for #481, a recompilation bug with Template Haskell

test('recomp009', [req_th, extra_files(['Main.hs', 'Sub1.hs', 'Sub2.hs'])], makefile_test, [])
