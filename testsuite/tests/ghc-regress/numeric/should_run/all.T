# Args to compile_and_run are:
#	extra compile flags
#	extra run flags
#	expected process return value, if not zero

test('arith001', normal, compile_and_run, [''])
test('arith002', normal, compile_and_run, [''])
test('arith003', normal, compile_and_run, [''])
test('arith004', normal, compile_and_run, [''])
test('arith005', normal, compile_and_run, [''])
test('arith006', normal, compile_and_run, [''])
test('arith007', normal, compile_and_run, [''])

# This test generates slightly different results with the NCG on x86,
# because of the extra precision when floating point computations are
# done in registers rather than going via memory.
if config.platform.startswith('i386-'):
	ways = expect_fail_for(['optasm','profasm'])
else:
	ways = normal

test('arith008', ways, compile_and_run, [''])

test('arith009', normal, compile_and_run, [''])
test('arith010', normal, compile_and_run, [''])
test('arith011', normal, compile_and_run, [''])

if config.platform.startswith('i386-'):
	ways = expect_fail_for(['optasm'])
else:
	ways = normal

test('arith012', ways, compile_and_run, [''])

test('arith013', only_compiler_types(['ghc']), compile_and_run, [''])
test('arith014', normal, compile_and_run, [''])
test('arith015', normal, compile_and_run, [''])
test('num009',   normal, compile_and_run, [''])
test('num010',   normal, compile_and_run, [''])
test('num011',   normal, compile_and_run, [''])
test('arith016', only_compiler_types(['ghc']), compile_and_run, ['-fglasgow-exts'])
test('arith017', normal, compile_and_run, [''])
