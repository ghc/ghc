test('T13786',
     [when(unregisterised(), fragile(17018)), req_c, when(opsys('linux') and not ghc_dynamic(), expect_broken(20706))],
     makefile_test, [])

