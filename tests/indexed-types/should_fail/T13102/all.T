# Test that we don't use a family instance that we didn't import

test('T13102', [expect_broken(13102),
     extra_files(['A.hs', 'B.hs', 'orphan'])],
     makefile_test, ['T13102'])
