test('memcpy',
     unless_platform('x86_64-unknown-linux',skip), compile_cmp_asm, [''])
test('memcpy-unroll',
     unless_platform('x86_64-unknown-linux',skip), compile_cmp_asm, [''])
test('memcpy-unroll-conprop',
     unless_platform('x86_64-unknown-linux',skip), compile_cmp_asm, [''])
