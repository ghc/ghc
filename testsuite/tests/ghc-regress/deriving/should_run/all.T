# Args to vt are:
#	extra compile flags
#	extra run flags
#	expected process return value, if not zero

test('drvrun001', skip_if_fast, compile_and_run, [''])
test('drvrun002', skip_if_fast, compile_and_run, [''])
test('drvrun003', skip_if_fast, compile_and_run, [''])
test('drvrun004', skip_if_fast, compile_and_run, [''])
test('drvrun005', skip_if_fast, compile_and_run, [''])
test('drvrun006', normal, compile_and_run, [''])
test('drvrun007', skip_if_fast, compile_and_run, [''])
test('drvrun008', skip_if_fast, compile_and_run, ['-funbox-strict-fields'])
test('drvrun009', skip_if_fast, compile_and_run, [''])
test('drvrun010', skip_if_fast, compile_and_run, [''])
test('drvrun011', skip_if_fast, compile_and_run, [''])
test('drvrun012', skip_if_fast, compile_and_run, [''])
test('drvrun013', skip_if_fast, compile_and_run, [''])
test('drvrun014', skip_if_fast, compile_and_run, [''])
test('drvrun015', skip_if_fast, compile_and_run, [''])
test('drvrun016', skip_if_fast, compile_and_run, ['-funbox-strict-fields'])
test('drvrun017', compose(skip_if_fast, only_compiler_types(['ghc'])), compile_and_run, [''])
test('drvrun018', skip_if_fast, compile_and_run, [''])
