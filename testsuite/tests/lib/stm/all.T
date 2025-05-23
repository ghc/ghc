test('T26028', only_ways(['threaded1']), compile_and_run, ['-O2'])
test('T26291a', normal, compile_and_run, ['-O2'])
test('T26291b', only_ways(['threaded1']), compile_and_run, ['-O2'])
