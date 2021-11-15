
test('T7835', [when(opsys("OSJavaScript"), skip), extra_files(['Test.hs', 'TestPrim.hs', 'test-prims.cmm'])], makefile_test, [])
