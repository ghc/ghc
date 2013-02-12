
# Args to compile_and_run are:
#		extra compile flags
#               extra run flags
#               expected process return value, if not zero

test('arr001', when(fast(), skip), compile_and_run, [''])
test('arr002', when(fast(), skip), compile_and_run, [''])
test('arr003', compose(when(fast(), skip),exit_code(1)), compile_and_run, [''])
test('arr004', compose(when(fast(), skip),exit_code(1)), compile_and_run, [''])
test('arr005', when(fast(), skip), compile_and_run, [''])
test('arr006', when(fast(), skip), compile_and_run, [''])
test('arr007', compose(when(fast(), skip),exit_code(1)), compile_and_run, [''])
test('arr008', compose(when(fast(), skip),exit_code(1)), compile_and_run, [''])
test('arr009', when(fast(), skip), compile_and_run, [''])
test('arr010', when(fast(), skip), compile_and_run, [''])
test('arr011', when(fast(), skip), compile_and_run, [''])
test('arr012', when(fast(), skip), compile_and_run, [''])
test('arr013', when(fast(), skip), compile_and_run, [''])
test('arr014', when(fast(), skip), compile_and_run, [''])
test('arr015', when(fast(), skip), compile_and_run, [''])
test('arr016', reqlib('random'), compile_and_run, [''])
test('arr017', when(fast(), skip), compile_and_run, [''])
test('arr018', when(fast(), skip), compile_and_run, [''])
test('arr019', normal, compile_and_run, [''])
test('arr020', normal, compile_and_run, [''])
