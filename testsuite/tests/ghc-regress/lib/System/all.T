
test('exitWith001', exit_code(42), compile_and_run, [''])
test('getArgs001',  normal, compile_and_run, [''])
test('getEnv001',   normal, compile_and_run, [''])

test('system001',   expect_fail_if_platform("i386-unknown-mingw32"), \
	compile_and_run, [''])

