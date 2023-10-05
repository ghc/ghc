test('determ021',
     [normalise_fun(normalise_errmsg),
      normalise_version('ghc-internal', 'base', 'ghc-prim'),
      extra_files(['A.hs'])],
     makefile_test, ['determ021'])
