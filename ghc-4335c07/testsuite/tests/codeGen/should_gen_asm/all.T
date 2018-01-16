is_amd64_codegen = [
    unless(platform('x86_64-unknown-linux'),skip),
    when(unregisterised, skip),
]

test('memcpy', is_amd64_codegen,  compile_cmp_asm, [''])
test('memcpy-unroll', is_amd64_codegen, compile_cmp_asm, [''])
test('memcpy-unroll-conprop', is_amd64_codegen, compile_cmp_asm, [''])
test('memset-unroll', is_amd64_codegen, compile_cmp_asm, [''])
