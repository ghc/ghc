test('echo001', set_stdin("echo001.hs"), compile_and_run, [''])

test('hTell001', normal, compile_and_run, [''])

test('hTell002', normal, compile_and_run, [''])

test('performGC001', normal, compile_and_run, [''])

# optimisation screws up this test because some of the traces get commoned up
test('trace001', normal, compile_and_run, [''])

test('hGetBuf002', normal, compile_and_run, [''])
test('hGetBuf003', normal, compile_and_run, [''])
test('hPutBuf001', normal, compile_and_run, [''])
test('hPutBuf002', extra_clean(['hPutBuf002.out']), compile_and_run, [''])
