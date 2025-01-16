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

test('FloatFnInverses', normal, compile_and_run, [''])

test('T1603', skip, compile_and_run, [''])
test('T3676', expect_broken(3676), compile_and_run, [''])
test('T4381', normal, compile_and_run, [''])
test('T4383', normal, compile_and_run, [''])

test('add2', normal, compile_and_run, ['-fobject-code'])
test('mul2', normal, compile_and_run, ['-fobject-code'])
test('mul2int', normal, compile_and_run, ['-fobject-code'])
test('quotRem2', normal, compile_and_run, ['-fobject-code'])
test('quotRem2Large', normal, compile_and_run, ['-fobject-code'])
test('T5863', normal, compile_and_run, [''])

test('T7014', js_skip, makefile_test, [])

test('T7233', normal, compile_and_run, [''])
test('NumDecimals', normal, compile_and_run, [''])
test('T8726', normal, compile_and_run, [''])
test('CarryOverflow', normal, compile_and_run, [''])
test('T9407', normal, compile_and_run, [''])
test('T9810', normal, compile_and_run, [''])
test('T10011', normal, compile_and_run, [''])
test('T10962', normal, compile_and_run, ['-O2'])
test('T11702', [unless(js_arch(),extra_ways(['optasm']))], compile_and_run, [''])
test('T12136', normal, compile_and_run, [''])
test('T15301', normal, compile_and_run, ['-O2'])
test('T497', normal, compile_and_run, ['-O'])
test('T17303', normal, compile_and_run, [''])
test('T18359', normal, compile_and_run, [''])
test('T18499', normal, compile_and_run, [''])
test('T18509', normal, compile_and_run, [''])
test('T18515', normal, compile_and_run, [''])
test('T18604', normal, compile_and_run, [''])
test('T19931', normal, compile_and_run, ['-O2'])

test('IntegerToFloat', normal, compile_and_run, [''])

test('T20291', normal, compile_and_run, [''])
test('T22282', normal, compile_and_run, [''])
test('T22671', js_fragile(24259), compile_and_run, [''])
test('foundation', [when(js_arch(), run_timeout_multiplier(2)), js_fragile(24259)], compile_and_run, ['-O -package transformers'])
test('T24066', normal, compile_and_run, [''])
test('T24245', normal, compile_and_run, [''])
