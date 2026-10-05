json_normalisers =  [ normalise_version('base', 'containers', 'template-haskell')
                    , normalise_slashes
                    , normalise_fun(normalise_dep_json)
                    ]

makefile_normalisers = [ normalise_slashes, normalise_fun(normalise_dep_makefile) ]

T24384_extras = [ extra_files(['T24384/*'])
                , pre_cmd('$MAKE -s --no-print-directory T24384_prep')
                ]

# These tests use a shell preprocessor that doesn't work on windows
test('T24384a', [ *T24384_extras, when(opsys('mingw32'), skip), *makefile_normalisers ], makefile_test, [])
test('T24384b', [ *T24384_extras, when(opsys('mingw32'), skip), *json_normalisers ], makefile_test, [])
test('T24384c', [ *T24384_extras, when(opsys('mingw32'), skip), *json_normalisers ], makefile_test, [])

# Multiple home units with explicit level imports.
test('mkdepend_mhu01',
     [ extra_files(['mhu01/']), dep_makefile, dep_json ],
     make_depend_multiunit, [['mhu01/unitA', 'mhu01/unitB', 'mhu01/unitC'], ''])
# Preprocessor must not be inherited by default active home unit
test('mkdepend_mhu02',
     [ extra_files(['mhu02/']), dep_json
     , when(opsys('mingw32'), skip) # shell script preprocessor
     ],
     make_depend_multiunit, [['mhu02/unitA', 'mhu02/unitB'], ''])
# A relative -pgmF with -working-dr
test('mkdepend_mhu03',
     [ extra_files(['mhu03/']), dep_json
     , when(opsys('mingw32'), skip) # shell script preprocessor
     ],
     make_depend_multiunit, [['mhu03/unitA'], ''])

# Honour -F
test('mkdepend01',
     [ extra_files(['mkdepend01/']), dep_makefile, dep_json ],
     make_depend, [['mkdepend01/A.hs'], '-pgmF pp'])

