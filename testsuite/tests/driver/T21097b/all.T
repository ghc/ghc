# Package b is unusable (broken dependency) and reexport Foo from a (which is usable)
test('T21097b',
  [ extra_files(["pkgdb", "pkgdb/a.conf", "pkgdb/b.conf", "Test.hs"])
  , ignore_stderr
  , exit_code(2)
  ], makefile_test, [])
