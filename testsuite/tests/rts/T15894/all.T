test('T15894',
     [extra_files(['copysign.c', 'main.hs']), when(ghc_dynamic(), skip)],
     makefile_test, ['T15894'])
