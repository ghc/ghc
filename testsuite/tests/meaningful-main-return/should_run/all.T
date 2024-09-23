test('T24848a', exit_code(0), compile_fail, [''])
test('T24848b', exit_code(0), compile_and_run, [''])
test('T24848c', exit_code(5), compile_and_run, [''])

#FIX: Can I run those two as the same test?
test('T24848d', exit_code(0), compile, [''])
test('T24848d2', exit_code(0), compile_and_run, [''])