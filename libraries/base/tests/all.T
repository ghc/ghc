
test('readFloat', exit_code(1), compile_and_run, [''])
test('enumDouble', normal, compile_and_run, [''])
test('enumRatio', normal, compile_and_run, [''])
test('tempfiles', normal, compile_and_run, [''])
test('fixed', normal, compile_and_run, [''])
test('quotOverflow', normal, compile_and_run, [''])
test('assert', exit_code(1), compile_and_run, ['-fno-ignore-asserts'])
