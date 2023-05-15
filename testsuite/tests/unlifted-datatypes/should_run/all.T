test('UnlData1', normal, compile_and_run, [''])
test('UnlGadt1', exit_code(1), compile_and_run, [''])
test('T23549', normal, multimod_compile_and_run, ['T23549', ''])
