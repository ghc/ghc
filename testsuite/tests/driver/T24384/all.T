test_extras =  extra_files(
  ['A.hs', 'lib/', 'C.hs', 'C.hs-boot', 'D.hs', 'E.hs',
  'dep', 'dep-internal', 'dep-public',
  'preproc.sh'
  ])

test('T24384a', [ test_extras, pre_cmd('$MAKE -s --no-print-directory T24384_prep') ], makefile_test, [])
test('T24384b', [ test_extras, pre_cmd('$MAKE -s --no-print-directory T24384_prep') ], makefile_test, [])
