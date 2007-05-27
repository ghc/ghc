setTestOpts(only_ways(['threaded1', 'threaded2']));

test('par001', reqlib('parallel'), compile_and_run, [''])
