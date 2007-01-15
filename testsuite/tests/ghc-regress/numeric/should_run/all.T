# Args to compile_and_run are:
#	extra compile flags
#	extra run flags
#	expected process return value, if not zero

# In fast mode, we do arith003 and arith011 only.

test('arith001', skip_if_fast, compile_and_run, [''])
test('arith002', skip_if_fast, compile_and_run, [''])
test('arith003', normal, compile_and_run, [''])
test('arith004', skip_if_fast, compile_and_run, [''])
test('arith005', skip_if_fast, compile_and_run, [''])
test('arith006', skip_if_fast, compile_and_run, [''])
test('arith007', skip_if_fast, compile_and_run, [''])

# This test generates slightly different results with the NCG on x86,
# because of the extra precision when floating point computations are
# done in registers rather than going via memory.
if config.platform.startswith('i386-'):
	ways = expect_fail_for(['optasm','profasm'])
else:
	ways = normal

test('arith008', compose(skip_if_fast,ways), compile_and_run, [''])

test('arith009', skip_if_fast, compile_and_run, [''])
test('arith010', skip_if_fast, compile_and_run, [''])
test('arith011', expect_broken_if_platform(1052, 'x86_64-unknown-linux'), compile_and_run, [''])

if config.platform.startswith('i386-'):
	ways = expect_fail_for(['optasm'])
else:
	ways = normal

test('arith012', compose(skip_if_fast,ways), compile_and_run, [''])

test('arith013', compose(skip_if_fast,only_compiler_types(['ghc'])), compile_and_run, [''])
test('arith014', skip_if_fast, compile_and_run, [''])
test('arith015', skip_if_fast, compile_and_run, [''])
test('num009',   skip_if_fast, compile_and_run, [''])
test('num010',   skip_if_fast, compile_and_run, [''])
test('num011',   skip_if_fast, compile_and_run, [''])
test('num012',   skip_if_fast, compile_and_run, [''])
test('num013',   skip_if_fast, compile_and_run, [''])
test('arith016', compose(skip_if_fast,only_compiler_types(['ghc'])), compile_and_run, ['-fglasgow-exts'])
test('arith017', skip_if_fast, compile_and_run, [''])
test('arith018', skip_if_fast, compile_and_run, [''])
test('arith019', skip_if_fast, compile_and_run, [''])
test('expfloat', skip_if_fast, compile_and_run, ['-fglasgow-exts'])
