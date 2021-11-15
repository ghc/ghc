test('T13786',
     [skip, when(unregisterised(), fragile_for(17018, ['ghci']))],
     makefile_test, [])

