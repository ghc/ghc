test('simpl001', normal, compile, [''])
test('simpl002', normal, compile, [''])
test('simpl003', normal, compile, [''])
test('simpl004', normal, compile, ['-package lang'])
test('simpl005', normal, compile, [''])

# these two are really a multi-module test
test('Simpl006Help', normal, compile, [''])
test('simpl006', normal, compile, ['-package concurrent'])

test('simpl007', normal, compile, [''])
test('simpl008', normal, compile, ['-fglasgow-exts'])

# these two are really a multi-module test
test('Simpl009Help', normal, compile, [''])
test('simpl009', normal, compile, [''])
