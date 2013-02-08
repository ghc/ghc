
# We need to run prof001 and prof002 the normal way, as the extra flags
# added for the profiling ways makes it pass
test('prof001', compose(only_ways(['normal']), req_profiling), compile_and_run, ['-prof -caf-all'])
test('prof002', compose(only_ways(['normal']), req_profiling), compile_and_run, ['-prof -caf-all'])

test('T2410', compose(only_ways(['normal']), req_profiling), compile, ['-O2 -prof -caf-all'])

