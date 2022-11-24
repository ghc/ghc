test('T13786',
     [when(unregisterised(), fragile(17018)), js_broken(22359), when(opsys('linux') and not ghc_dynamic(), expect_broken(20706))],
     makefile_test, [])

