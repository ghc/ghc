
test('read001', normal, compile, [''])
test('read002', normal, compile, [''])
test('read003', normal, compile, [''])
test('read004', normal, compile, [''])
test('read005', normal, compile, [''])

# Missing:
# test('read006', normal, compile, [''])
# test('read007', normal, compile, [''])

test('read008', normal, compile, [''])
test('read009', normal, compile, [''])
test('read010', normal, compile, [''])
test('read011', normal, compile, [''])

# Missing:
# test('read012', normal, compile, [''])
# test('read013', normal, compile, [''])

test('read014', normal, compile, ['-Wall -fno-warn-orphans'])
test('read015', normal, compile, [''])
test('read016', normal, compile, [''])
test('read017', normal, compile, [''])
test('read018', normal, compile, [''])
test('read019', normal, compile, [''])

# Missing:
# test('read020', normal, compile, [''])

test('read021', normal, compile, [''])
test('read022', normal, compile, ["-fglasgow-exts"])
test('read023', expect_fail_if_compiler_type('hugs'), compile, [''])
test('read024', normal, compile, ["-fglasgow-exts"])
test('read025', normal, compile, [''])
test('read026', normal, compile, [''])
test('read027', normal, compile, [''])
test('read028', normal, compile, [''])
test('read029', normal, compile, [''])
test('read030', normal, compile, [''])
test('read031', normal, compile, [''])
test('read032', normal, compile, [''])
test('read033', expect_fail_if_compiler_type('hugs'), compile, [''])
test('read034', expect_fail_if_compiler_type('hugs'), compile, [''])
test('read036', normal, compile, [''])
test('read037', normal, compile, [''])
test('read038', normal, compile, [''])
test('read039', normal, compile, ['-fno-code'])
test('read040', normal, compile, [''])
test('read041', literate, compile, [''])
test('read042', normal, compile, [''])
test('read043', normal, compile, [''])
