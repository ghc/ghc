# Args to compile_and_run are:
#	extra compile flags
#	extra run flags
#	expected process return value, if not zero

test('arith001', normal, compile_and_run, [''])
test('arith002', normal, compile_and_run, [''])
test('arith003', normal, compile_and_run, [''])
test('arith004', normal, compile_and_run, [''])
test('arith005', when(platform('i386-apple-darwin'), expect_broken_for(7043, 'ghci')), compile_and_run, [''])
test('arith006', normal, compile_and_run, [''])
test('arith007', normal, compile_and_run, [''])

# On i386, we need -msse2 to get reliable floating point results
if config.arch == 'i386':
    opts = '-msse2'
else:
    opts = ''
test('arith008', normal, compile_and_run, [opts])

test('arith009', normal, compile_and_run, [''])
test('arith010', normal, compile_and_run, [''])
test('arith011', normal, compile_and_run, [''])

test('arith012', normal, compile_and_run, [opts])

test('arith013', normal, compile_and_run, [''])
test('arith014', normal, compile_and_run, [''])
test('arith015', normal, compile_and_run, [''])
test('numrun009', normal, compile_and_run, [''])
test('numrun010', normal, compile_and_run, [''])
test('numrun011', normal, compile_and_run, [''])
test('numrun012', normal, compile_and_run, [''])
test('numrun013', normal, compile_and_run, [''])
test('numrun014', normal, compile_and_run, [''])
test('numrun015', normal, compile_and_run, [''])
test('numrun016', normal, compile_and_run, [''])
test('arith016', normal, compile_and_run, [''])
test('arith017', normal, compile_and_run, [''])
test('arith018', normal, compile_and_run, [''])
test('arith019', normal, compile_and_run, [''])
test('expfloat', normal, compile_and_run, [''])

test('T1603', skip, compile_and_run, [''])
test('T3676', expect_broken(3676), compile_and_run, [''])
test('T4381', normal, compile_and_run, [''])
test('T4383', normal, compile_and_run, [''])

test('add2', normal, compile_and_run, ['-fobject-code'])
test('mul2', normal, compile_and_run, ['-fobject-code'])
test('quotRem2', normal, compile_and_run, ['-fobject-code'])
test('T5863', normal, compile_and_run, [''])

test('T7014', [], run_command, ['$MAKE -s --no-print-directory T7014'])

test('T7233', normal, compile_and_run, [''])
test('NumDecimals', normal, compile_and_run, [''])
test('T8726', normal, compile_and_run, [''])
test('CarryOverflow', omit_ways(['ghci']), compile_and_run, [''])
test('T9407', normal, compile_and_run, [''])
test('T9810', normal, compile_and_run, [''])
test('T10011', normal, compile_and_run, [''])
test('T10962', omit_ways(['ghci']), compile_and_run, [''])
test('T11702', extra_ways(['optasm']), compile_and_run, [''])
test('T12136', normal, compile_and_run, [''])
