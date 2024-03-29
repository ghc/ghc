
test('hsc2hs001', js_broken(22355), makefile_test, [])

test('hsc2hs002', js_broken(22355), makefile_test, [])

test('hsc2hs003', js_broken(22355), makefile_test, [])

test('hsc2hs004', [], makefile_test, [])


test('T3837', js_broken(22355), makefile_test, [])

# These are broken on CI (and potentially elsewhere) since we
# are building ARMv7 # binaries on an AArch64 machine.
test('T4340', [ when(arch('arm'), expect_broken(17556))
              , js_broken(17556)
              ], makefile_test, [])
test('T10272', [ when(arch('arm'), expect_broken(17556))
               , js_broken(17556)
               ], makefile_test, [])

test('T11004', js_broken(22355), makefile_test, [])

test('T12504', [extra_files(['T12504']), ignore_stdout, js_broken(22355)], makefile_test, [])

# Make sure response files are read and used.
test('T15758', js_broken(22355), makefile_test, [])
