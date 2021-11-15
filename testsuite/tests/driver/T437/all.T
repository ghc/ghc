# Test for #437, a recompilation bug with '-main-is'

test('T437', [when(opsys("OSJavaScript"), skip), extra_files(['Test.hs', 'Test2.hs'])], makefile_test, [])
