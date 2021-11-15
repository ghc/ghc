test('T1959', [when(opsys("OSJavaScript"), skip), extra_files(['B.hs', 'C.hs', 'D.hs', 'E1.hs', 'E2.hs'])], makefile_test, ['dotest'])
