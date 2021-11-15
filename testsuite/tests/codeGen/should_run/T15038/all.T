test('T15038',
     [when(opsys("OSJavaScript"), skip), reqlib('containers'), reqlib('ghc-prim'), reqlib('primitive')],
     makefile_test, [])
