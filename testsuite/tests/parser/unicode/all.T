# test that we catch UTF-8 decoding errors

test('utf8_002', normal, compile_fail, [''])
test('utf8_003', normal, compile_fail, [''])
test('utf8_004', normal, compile_fail, [''])
test('utf8_005', normal, compile_fail, [''])

test('utf8_010', normal, compile_fail, [''])
test('utf8_011', normal, compile_fail, [''])

test('utf8_020', normal, compile_fail, [''])
test('utf8_021', normal, compile_fail, [''])
test('utf8_022', normal, compile_fail, [''])

# test that we can understand unicode characters in lexemes

test('utf8_024', normal, compile_and_run, [''])

test('T1744', normal, compile_and_run, [''])
test('T1103', normal, compile, [''])
test('T2302', only_ways(['normal']), compile_fail, [''])
test('T4373', normal, compile, [''])
test('T6016', [], compile_and_run, ['-package ghc'])
test('T7671', normal, compile, [''])
# TODO: This test ought to be run in a non-UTF8 locale, but this is not yet
# supported by the test suite (see 10907)
test('T10907', normal, compile, [''])
test('T7650', normal, compile, [''])

test('brackets', normal, compile, [''])
test('T18225A', req_th, compile, [''])
test('T18225B', normal, compile_fail, [''])

test('T18158', normal, compile, [''])
test('T18158b', normal, compile_fail, [''])

test('splice', normal, compile_and_run, [''])
