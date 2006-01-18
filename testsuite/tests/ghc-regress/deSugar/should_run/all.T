# Args to compile_and_run are:
#	extra compile flags
#	extra run flags
#	expected process return value, if not zero

test('dsrun001', normal, compile_and_run, [''])
test('dsrun002', normal, compile_and_run, [''])
test('dsrun003', normal, compile_and_run, [''])
test('dsrun004', normal, compile_and_run, [''])
test('dsrun005', exit_code(1), compile_and_run, [''])
test('dsrun006', normal, compile_and_run, [''])
test('dsrun007', exit_code(1), compile_and_run, [''])
test('dsrun008', exit_code(1), compile_and_run, [''])
test('dsrun009', normal, compile_and_run, [''])
test('dsrun010', normal, compile_and_run, [''])
test('dsrun011', skip_if_fast, compile_and_run, [''])
test('dsrun012', skip_if_fast, compile_and_run, [''])
test('dsrun013', normal, compile_and_run, [''])
