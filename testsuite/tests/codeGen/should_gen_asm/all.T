test('memcpy', unless_arch('x86_64',skip), compile_cmp_asm, [''])
test('memcpy-unroll', unless_arch('x86_64',skip), compile_cmp_asm, [''])
test('memcpy-unroll-conprop', unless_arch('x86_64',skip), compile_cmp_asm, [''])
