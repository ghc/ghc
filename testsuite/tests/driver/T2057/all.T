test(
  'T2057',
  [ extra_files(['pkgA1', 'pkgA2', 'pkgB', 'app', 'README.md'])
  , ignore_stdout
  ],
  makefile_test,
  []
)
