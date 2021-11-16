test('T16586', normal, compile_and_run, ['-O2'])
# Event-manager not supported on Windows
test('T16916', [javascript_failures, when(opsys('mingw32'), skip)], compile_and_run, ['-O2 -threaded'])
