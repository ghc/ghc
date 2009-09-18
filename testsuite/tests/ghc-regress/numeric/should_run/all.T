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
	ways = expect_fail_for(['optasm','threaded2','hpc','dyn'])
else:
	ways = normal

test('arith008', compose(normal,ways), compile_and_run, [''])

test('arith009', normal, compile_and_run, [''])
test('arith010', normal, compile_and_run, [''])
test('arith011', normal, compile_and_run, [''])

if config.platform.startswith('i386-'):
	ways = expect_fail_for(['optasm','threaded2','hpc','dyn'])
else:
	ways = normal

test('arith012', compose(normal,ways), compile_and_run, [''])

test('arith013', compose(normal,only_compiler_types(['ghc'])), compile_and_run, [''])
test('arith014', normal, compile_and_run, [''])
test('arith015', normal, compile_and_run, [''])
test('num009',   normal, compile_and_run, [''])
test('num010',   normal, compile_and_run, [''])
test('num011',   normal, compile_and_run, [''])
test('num012',   normal, compile_and_run, [''])
test('num013',   normal, compile_and_run, [''])
test('num014',   normal, compile_and_run, [''])
test('arith016', compose(normal,only_compiler_types(['ghc'])), compile_and_run, ['-fglasgow-exts'])
test('arith017', normal, compile_and_run, [''])
test('arith018', normal, compile_and_run, [''])
test('arith019', normal, compile_and_run, [''])
test('expfloat', normal, compile_and_run, ['-fglasgow-exts'])

test('1603', skip, compile_and_run, ['-fglasgow-exts'])
