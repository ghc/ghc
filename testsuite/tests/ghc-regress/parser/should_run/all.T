test('read001', normal, compile_and_run, [''])
test('read002', normal, compile_and_run, [''])
test('read003', normal, compile_and_run, [''])
test('read004', expect_broken_for(1257, ['ghci']), compile_and_run, [''])
