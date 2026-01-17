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

test('msse-option-order', [unless(arch('x86_64') or arch('i386'), skip),
                           when(unregisterised(), skip)], compile_grep_asm, ['hs', False, '-msse4.2 -msse2'])
test('mavx-should-enable-popcnt', [unless(arch('x86_64') or arch('i386'), skip),
                                   when(unregisterised(), skip)], compile_grep_asm, ['hs', False, '-mavx'])
test('avx512-int64-mul', [unless(arch('x86_64'), skip),
                          when(unregisterised(), skip)], compile_grep_asm, ['hs', True, '-mavx512dq -mavx512vl'])
test('avx512-int64-minmax', [unless(arch('x86_64'), skip),
                             when(unregisterised(), skip)], compile_grep_asm, ['hs', True, '-mavx512vl'])
test('avx512-word64-minmax', [unless(arch('x86_64'), skip),
                              when(unregisterised(), skip)], compile_grep_asm, ['hs', True, '-mavx512vl'])
