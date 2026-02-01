setTestOpts(only_ways(['normal']));

qextra_files = extra_files(['Example'])

test('qstrings_multiline_no_ext', normal, compile_fail, ['-v0'])
test('qstrings_bad_expr', [qextra_files], multimod_compile_fail, ['qstrings_bad_expr', '-v0'])
test('qstrings_bad_pat', [qextra_files], multimod_compile_fail, ['qstrings_bad_pat', '-v0'])
