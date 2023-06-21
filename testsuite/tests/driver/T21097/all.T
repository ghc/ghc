# Package a and b both depend on c which is broken (depends on non-existing d)
test('T21097',
  [ extra_files(["pkgdb", "pkgdb/a.conf", "pkgdb/b.conf", "pkgdb/c.conf", "Test.hs"])
  ], makefile_test, [])
