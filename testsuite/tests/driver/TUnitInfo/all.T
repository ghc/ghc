# A test for checking the number of live GenericUnitInfo objects
test('generic-unit-info-space-single'
    , [ extra_files(['Foo.hs', 'generic-unit-info-space.hs'])
      , pre_cmd('$MAKE -s --no-print-directory generic-unit-info-space-single_Prep')
      # On js_arch, crashes with:
      # ReferenceError: h$requestHeapCensus is not defined
      , when(js_arch(), skip)
      ]
    , makefile_test
    , ['generic-unit-info-space-single'])

# A test for checking the number of live GenericUnitInfo objects
test('generic-unit-info-space-mhu'
    , [ extra_files(['genMhu.sh', 'generic-unit-info-space.hs'])
      , pre_cmd('$MAKE -s --no-print-directory generic-unit-info-space-mhu_Prep')
      # On js_arch, crashes with:
      # ReferenceError: h$requestHeapCensus is not defined
      , when(js_arch(), skip)
      ]
    , makefile_test
    , ['generic-unit-info-space-mhu'])
