test('simpl001', normal, compile, [''])
test('simpl002', normal, compile, [''])
test('simpl003', normal, compile, [''])
test('simpl004', only_compiler_types(['ghc']), compile, [''])
test('simpl005', normal, compile, [''])

# Missing:
# test('simpl006', normal, compile, [''])

test('simpl007', normal, compile, [''])
# Missing: simpl008
test('simpl009', normal, compile, [''])
test('simpl010', normal, compile, [''])
test('simpl011', normal, compile, [''])
test('simpl012', normal, compile, [''])
test('simpl013', normal, compile, [''])
test('simpl014', normal, compile, [''])
test('simpl015', only_ways(['optc','optasm']), compile, [''])
test('simpl016', normal, compile, [''])
test('simpl017', normal, compile, [''])
test('simpl018', normal, compile, [''])
test('simpl019', normal, compile, [''])
test('simpl020',
     extra_clean(['Simpl020_A.hi', 'Simpl020_A.o']),
     multimod_compile,
     ['simpl020', '-v0 -XFlexibleInstances -XUndecidableInstances'])

test('simpl-T1370', normal, compile, [''])
test('T2520', normal, compile, [''])


test('spec001', skip_if_fast, compile, [''])
test('spec002', normal, compile, [''])
test('spec003', normal, compile, [''])

test('rule1', normal, compile, [''])

test('strict-float', normal, compile, [''])

