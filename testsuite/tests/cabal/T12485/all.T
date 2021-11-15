test('T12485', [when(opsys("OSJavaScript"), skip), extra_files(['a.pkg', 'b.pkg', 'Main.hs'])], makefile_test, [])
