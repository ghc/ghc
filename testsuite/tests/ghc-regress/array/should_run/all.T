
# Args to compile_and_run are:
#		extra compile flags
#               extra run flags
#               expected process return value, if not zero

test('arr001', skip_if_fast, compile_and_run, [''])
test('arr002', skip_if_fast, compile_and_run, [''])
test('arr003', compose(skip_if_fast,exit_code(1)), compile_and_run, [''])
test('arr004', compose(skip_if_fast,exit_code(1)), compile_and_run, [''])
test('arr005', skip_if_fast, compile_and_run, [''])
test('arr006', skip_if_fast, compile_and_run, [''])
test('arr007', compose(skip_if_fast,exit_code(1)), compile_and_run, [''])
test('arr008', compose(skip_if_fast,exit_code(1)), compile_and_run, [''])
test('arr009', skip_if_fast, compile_and_run, [''])
test('arr010', skip_if_fast, compile_and_run, [''])
test('arr011', skip_if_fast, compile_and_run, [''])
test('arr012', skip_if_fast, compile_and_run, [''])
test('arr013', skip_if_fast, compile_and_run, [''])
test('arr014', skip_if_fast, compile_and_run, [''])
test('arr015', skip_if_fast, compile_and_run, [''])
test('arr016', normal, compile_and_run, ['-fglasgow-exts'])
test('arr017', skip_if_fast, compile_and_run, [''])
