setTestOpts(only_ways(['normal']));

qextra_files = extra_files(['Example'])

test('qstrings_redundant_pattern', [qextra_files], multimod_compile, ['qstrings_redundant_pattern', '-v0'])
