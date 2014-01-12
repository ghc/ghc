test('memcpy',
     unless(platform('x86_64-unknown-linux'),skip), compile_cmp_asm, [''])
test('memcpy-unroll',
     unless(platform('x86_64-unknown-linux'),skip), compile_cmp_asm, [''])
test('memcpy-unroll-conprop',
     unless(platform('x86_64-unknown-linux'),skip), compile_cmp_asm, [''])
