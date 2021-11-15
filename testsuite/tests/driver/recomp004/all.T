test('recomp004', [when(opsys("OSJavaScript"), skip), extra_files(['Main.hs', 'c.h', 'c1.c', 'c2.c'])],
     makefile_test, [])
