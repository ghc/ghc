is_amd64_codegen = [
    unless(platform('x86_64-unknown-linux'),skip),
    when(unregisterised(), skip),
]

test('memcpy', is_amd64_codegen,  compile_cmp_asm, ['cmm', ''])
test('memcpy-unroll', is_amd64_codegen, compile_cmp_asm, ['cmm', ''])
test('memcpy-unroll-conprop', is_amd64_codegen, compile_cmp_asm, ['cmm', ''])
test('memset-unroll', is_amd64_codegen, compile_cmp_asm, ['cmm', ''])
test('bytearray-memset-unroll', is_amd64_codegen, compile_grep_asm, ['hs', True, ''])
test('bytearray-memcpy-unroll', is_amd64_codegen, compile_grep_asm, ['hs', True, ''])
test('T18137', [when(opsys('darwin'), skip), only_ways(llvm_ways)], compile_grep_asm, ['hs', False, '-fllvm -split-sections'])

test('T24941', [only_ways(['optasm'])], compile, ['-fregs-graph'])
is_aarch64_codegen = [
    unless(arch('aarch64'), skip),
    when(unregisterised(), skip),
]

# AArch64-specific tests
test('aarch64-ushr-subword', is_aarch64_codegen, compile_grep_asm, ['hs', True, '-O'])
test('aarch64-shl-subword', is_aarch64_codegen, compile_grep_asm, ['hs', True, '-O'])
