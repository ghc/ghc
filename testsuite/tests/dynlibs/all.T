
test('T3807', [req_shared_libs, when(opsys('mingw32'), skip)], makefile_test, [])

test('T4464', [req_shared_libs, unless(opsys('mingw32'), skip)], makefile_test, [])

test('T5373', [req_shared_libs], makefile_test, [])

# It's not clear exactly what platforms we can expect this to succeed on.
test('T13702', unless(opsys('linux'), skip), makefile_test, [])
