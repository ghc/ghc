test('rn.prog006', [when(opsys("OSJavaScript"), skip), extra_files(['A.hs', 'B/', 'Main.hs', 'pwd.hs'])], makefile_test, [])
