test('T3807',
    [req_shared_libs,
     when(have_thread_sanitizer(), expect_broken(18883)),
     when(opsys('mingw32'),
     skip)], makefile_test, [])

test('T4464', [req_shared_libs, unless(opsys('mingw32'), skip)], makefile_test, [])

test('T5373', [req_shared_libs], makefile_test, [])

# It's not clear exactly what platforms we can expect this to succeed on.
test('T13702',
     [when(have_thread_sanitizer(), expect_broken(18884)),
      req_shared_libs, unless(opsys('linux'), skip)],
     makefile_test, [])

# test that -shared and -flink-rts actually links the rts
test('T18072', [req_shared_libs, unless(opsys('linux'), skip)], makefile_test, [])

# test that -shared and -flink-rts respects alternative RTS flavours
test('T18072debug', [extra_files(['T18072.hs']), req_shared_libs, unless(opsys('linux'), skip)], makefile_test, [])

# check that -staticlib and -fno-link-rts results in an archive without the RTR library 
test('T18072static', [extra_files(['T18072.hs']), unless(opsys('linux'), skip)], makefile_test, [])
