
# Missing:
# test('cc002', normal, compile_fail, [''])

test('ccfail001', normal, compile_fail, [''])
test('ccfail002', normal, compile_fail, [''])
test('ccfail003', normal, compile_fail, [''])
test('T3066', normal, compile_fail, [''])
test('ccfail004', [extra_files(['Ccfail004A.hs'])], multimod_compile_fail, ['ccfail004', '-v0'])
test('ccfail005', normal, compile_fail, [''])
test('ccall_value', normal, compile_fail, [''])
test('capi_value_function', normal, compile_fail, [''])
test('NonreducingFfiSignature', normal, compile_fail, [''])
test('T5664', normal, compile_fail, ['-v0'])
test('T7506', normal, compile_fail, [''])
test('T7243', normal, compile_fail, [''])
test('T10461', normal, compile_fail, [''])
test('T16702', normal, compile_fail, [''])

# UnsafeReenter tests implementation of an undefined behavior (calling Haskell
# from an unsafe foreign function) and only makes sense in non-threaded way
# (threaded runtime will hang). See #14912.
test('UnsafeReenter',
     [omit_ways(threaded_ways), exit_code(1)],
     compile_and_run,
     ['UnsafeReenterC.c'])
