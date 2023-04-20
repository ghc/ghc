test('T15894',
     [ extra_files(['copysign.c', 'main.hs']), when(ghc_dynamic(), skip)
     , req_c
     ],
     makefile_test, ['T15894'])
