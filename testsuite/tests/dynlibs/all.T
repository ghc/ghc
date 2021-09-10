test('T3807',
     [unless(have_dynamic(), skip),
      when(have_thread_sanitizer(), expect_broken(18883))],
    makefile_test, [])

test('T4464', [req_dynamic_hs, unless(opsys('mingw32'), skip)], makefile_test, [])

test('T5373', [unless(have_dynamic(), skip)], makefile_test, [])

# It's not clear exactly what platforms we can expect this to succeed on.
test('T13702',
     [when(have_thread_sanitizer(), expect_broken(18884)),
      unless(have_dynamic(), skip),
      unless(opsys('linux'), skip)],
     makefile_test, [])

# test that -shared and -flink-rts actually links the RTS
test('T18072', [unless(have_dynamic(), skip), unless(opsys('linux'), skip)], makefile_test, [])

# test that -shared and -flink-rts respects alternative RTS flavours
test('T18072debug',
     [unless(have_dynamic(), skip),
      extra_files(['T18072.hs']),
      unless(opsys('linux'), skip)],
     makefile_test, [])

# check that -staticlib and -fno-link-rts results in an archive without the RTS library 
test('T18072static',
     [extra_files(['T18072.hs']),
      unless(opsys('linux'), skip)],
     makefile_test, [])
