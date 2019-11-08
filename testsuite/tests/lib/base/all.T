test('T16586', normal, compile_and_run, ['-O2'])
test('T16916',
     when(opsys('mingw32'), skip), # Tests Unix event manager
     compile_and_run,
     ['-O2 -threaded'])
