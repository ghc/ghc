# N.B. this package requires a dynamically-linked ghc-bin, since it assumes
# that TH evaluation will build dynamic objects.
test('T20348', [extra_files(['A.hs']), unless(have_dynamic(), skip)], makefile_test, [])
test('T20348A', [extra_files(['A.hs']), unless(have_dynamic(), skip)], makefile_test, [])
test('T20348B', [extra_files(['A.hs']), unless(have_dynamic(), skip)], makefile_test, [])
test('T20348C', [extra_files(['A.hs']), unless(have_dynamic(), skip)], makefile_test, [])
test('T20348D', [extra_files(['A.hs']), unless(have_dynamic(), skip)], makefile_test, [])
test('T20348E', [extra_files(['A.hs']), unless(have_dynamic(), skip)], makefile_test, [])
