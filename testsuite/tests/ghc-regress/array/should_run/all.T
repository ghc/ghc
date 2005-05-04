
# Args to compile_and_run are:
#		extra compile flags
#               extra run flags
#               expected process return value, if not zero

test('arr001', normal, compile_and_run, [''])
test('arr002', normal, compile_and_run, [''])
test('arr003', exit_code(1), compile_and_run, [''])
test('arr004', exit_code(1), compile_and_run, [''])
test('arr005', normal, compile_and_run, [''])
test('arr006', normal, compile_and_run, [''])
test('arr007', exit_code(1), compile_and_run, [''])
test('arr008', exit_code(1), compile_and_run, [''])
test('arr009', normal, compile_and_run, [''])
test('arr010', normal, compile_and_run, [''])
test('arr011', normal, compile_and_run, [''])
test('arr012', normal, compile_and_run, [''])
test('arr013', normal, compile_and_run, [''])
test('arr014', normal, compile_and_run, [''])
test('arr015', normal, compile_and_run, [''])
test('arr016', normal, compile_and_run, ['-fglasgow-exts'])
test('arr017', normal, compile_and_run, [''])
