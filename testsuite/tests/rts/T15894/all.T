test('T15894',
     [ extra_files(['copysign.c', 'main.hs']), when(ghc_dynamic(), skip)
     , js_broken(22359)
     ],
     makefile_test, ['T15894'])
