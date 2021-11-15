test('T15894',
     [when(opsys("OSJavaScript"), skip), extra_files(['copysign.c', 'main.hs']), when(ghc_dynamic(), skip)],
     makefile_test, ['T15894'])
