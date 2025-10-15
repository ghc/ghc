setTestOpts(only_ways(['normal']));

qextra_files = extra_files(['Example'])

test('qstrings_run_001', [qextra_files], multimod_compile_and_run, ['qstrings_run_001', ''])
