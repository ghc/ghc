def normalise_t21097b_output(s):
  res = ""
  for l in s.splitlines():
    if 'Foo' in l:
      res += l
      res += "\n"
  return res

# Package b is unusable (broken dependency) and reexport Foo from a (which is usable)
test('T21097b',
  [ extra_files(["pkgdb", "pkgdb/a.conf", "pkgdb/b.conf", "Test.hs"])
  , ignore_stderr
  , normalise_fun(normalise_t21097b_output)
  , exit_code(2)
  ], makefile_test, [])
