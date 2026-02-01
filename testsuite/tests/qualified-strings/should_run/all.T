setTestOpts(only_ways(['normal']));

qextra_files = extra_files(['Example'])

test('qstrings_expr', [qextra_files], multimod_compile_and_run, ['qstrings_expr', ''])
test('qstrings_pat', [qextra_files], multimod_compile_and_run, ['qstrings_pat', ''])
test('qstrings_th', [qextra_files], multimod_compile_and_run, ['qstrings_th', ''])
