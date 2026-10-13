test_extras = extra_files(
  ['A.hs', 'lib/', 'C.hs', 'C.hs-boot', 'D.hs', 'E.hs',
  'foo.h',
  'dep', 'dep-internal', 'dep-public',
  'preproc.sh'
  ])

normalisers = normalise_version('base', 'containers', 'template-haskell')

test('T24384a', [ test_extras, normalisers, pre_cmd('$MAKE -s --no-print-directory T24384_prep') ], makefile_test, [])
