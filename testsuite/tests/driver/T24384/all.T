
json_normalisers =  [ normalise_version('base', 'containers', 'template-haskell')
                    , normalise_slashes
                    , normalise_fun(normalise_dep_json)
                    ]

makefile_normalisers = [ normalise_slashes, normalise_fun(normalise_dep_makefile) ]

test_extras = extra_files(
  ['A.hs', 'lib/', 'C.hs', 'C.hs-boot', 'D.hs', 'E.hs',
  'foo.h',
  'dep', 'dep-internal', 'dep-public',
  'preproc.sh'
  ])

skip_on_mingw = when(opsys('mingw32'), skip)

test('T24384a', [ test_extras, pre_cmd('$MAKE -s --no-print-directory T24384_prep'), *makefile_normalisers ], makefile_test, [])
test('T24384b', [ test_extras, pre_cmd('$MAKE -s --no-print-directory T24384_prep'), skip_on_mingw, *json_normalisers ], makefile_test, [])
test('T24384c', [ test_extras, pre_cmd('$MAKE -s --no-print-directory T24384_prep'), skip_on_mingw, *json_normalisers ], makefile_test, [])
