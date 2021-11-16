
test('hsc2hs001', [], makefile_test, [])

test('hsc2hs002', [], makefile_test, [])

test('hsc2hs003', [], makefile_test, [])

test('hsc2hs004', [], makefile_test, [])


test('T3837', [], makefile_test, [])

test('T4340', [javascript_failures], makefile_test, [])

test('T10272', [javascript_failures], makefile_test, [])

test('T11004', [], makefile_test, [])

test('T12504', [extra_files(['T12504']), ignore_stdout], makefile_test, [])

# Make sure response files are read and used.
test('T15758', [], makefile_test, [])
