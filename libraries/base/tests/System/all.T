
test('exitWith001', exit_code(42), compile_and_run, [''])
test('getArgs001',  normal, compile_and_run, [''])
test('getEnv001',   normal, compile_and_run, [''])
test('T5930',   normal, compile_and_run, [''])

test('system001', [when(opsys("mingw32"), skip), req_process], \
	compile_and_run, [''])
test('Timeout001', js_broken(22261), compile_and_run, [''])
test('T16466', normal, compile_and_run, [''])
test('T23399', normal, compile_and_run, [''])
