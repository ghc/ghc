
test('T3807', [req_shared_libs, when(opsys('mingw32'), skip)], makefile_test, [])

test('T4464', [req_shared_libs, unless(opsys('mingw32'), skip)], makefile_test, [])

test('T5373', [req_shared_libs], makefile_test, [])

# It's not clear exactly what platforms we can expect this to succeed on.
test('T13702', unless(opsys('linux'), skip), makefile_test, [])

# test that -fshared-link-rts actually links the rts
test('T18072', [req_shared_libs,unless(opsys('linux'), skip)], makefile_test, [])
# test that -fshared-link-rts respects alternative RTS flavours
test('T18072debug', [extra_files(['T18072.hs']), req_shared_libs,unless(opsys('linux'), skip)], makefile_test, [])
test('T18072eventlog', [extra_files(['T18072.hs']), req_shared_libs,unless(opsys('linux'), skip)], makefile_test, [])
