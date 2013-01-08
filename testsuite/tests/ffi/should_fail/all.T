
# Missing:
# test('cc002', normal, compile_fail, [''])

test('ccfail001', only_compiler_types(['ghc']), compile_fail, [''])
test('ccfail002', only_compiler_types(['ghc']), compile_fail, [''])
test('ccfail003', only_compiler_types(['ghc']), compile_fail, [''])
test('T3066', only_compiler_types(['ghc']), compile_fail, [''])
test('ccfail004', only_compiler_types(['ghc']), multimod_compile_fail, ['ccfail004', '-v0'])
test('ccfail005', only_compiler_types(['ghc']), compile_fail, [''])
test('ccall_value', normal, compile_fail, [''])
test('capi_value_function', normal, compile_fail, [''])
test('T5664', normal, compile_fail, ['-v0'])
test('T7506', normal, compile_fail, [''])
