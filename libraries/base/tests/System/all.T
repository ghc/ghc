
test('exitWith001', exit_code(42), compile_and_run, [''])
test('getArgs001',  normal, compile_and_run, [''])
test('getEnv001',   normal, compile_and_run, [''])
test('T5930',   normal, compile_and_run, [''])

test('system001',   when(opsys("mingw32"), expect_fail), \
	compile_and_run, [''])
test('Timeout001', normal, compile_and_run, [''])
