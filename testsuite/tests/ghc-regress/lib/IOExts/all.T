test('echo001', set_stdin("echo001.hs"), compile_and_run, [''])

test('hTell001', expect_fail_if_platform("i386-unknown-mingw32"), \
	compile_and_run, [''])

test('hTell002', expect_fail_if_platform("i386-unknown-mingw32"), \
	compile_and_run, [''])

test('performGC001', normal, compile_and_run, [''])

# optimisation screws up this test because some of the traces get commoned up
test('trace001', normal, compile_and_run, [''])

test('hGetBuf001', normal, compile_and_run, [''])
test('hPutBuf001', normal, compile_and_run, [''])
test('hPutBuf002', normal, compile_and_run, [''])
clean(['hPutBuf002.out'])
