
test('read001', normal, compile_fail, [''])
test('read002', normal, compile_fail, [''])
test('read003', normal, compile_fail, [''])
test('read004', normal, compile_fail, [''])
test('read005', normal, compile_fail, [''])
test('read006', normal, compile_fail, [''])
test('read007', normal, compile_fail, [''])
test('read008', normal, compile_fail, [''])
test('read009', normal, compile_fail, [''])
# test10: missing
test('read011', normal, compile_fail, [''])
test('read012', normal, compile_fail, [''])
test('read013', normal, compile_fail, ['-fglasgow-exts'])
test('read014', normal, compile_fail, [''])
test('read015', normal, compile_fail, [''])
test('read016', normal, compile_fail, [''])
test('read017', normal, compile_fail, [''])
test('read018', normal, compile_fail, [''])
test('read019', normal, compile_fail, [''])
test('read020', normal, compile_fail, [''])

# empty file (length zero) is not a legal Haskell module.  It fails to compile
# because it doesn't contain a definition of Main.main.  GHC 5.02 crashed
# on this example.
test('read021', expect_fail_if_compiler_type('hugs'), compile_fail, [''])

test('read022', normal, compile_fail, [''])
test('read023', normal, compile_fail, [''])
test('read024', normal, compile_fail, [''])
test('read025', normal, compile_fail, [''])
test('read026', normal, compile_fail, [''])

# Fails in GHC 6.2+
test('read027', expect_broken_if_compiler_type(1060, 'ghc'), compile_fail, [''])

test('read028', normal, compile_fail, [''])
test('read029', normal, compile_fail, [''])
test('read030', normal, compile_fail, [''])
test('read031', normal, compile_fail, [''])
test('read032', expect_broken(314), compile_fail, ['-cpp'])
