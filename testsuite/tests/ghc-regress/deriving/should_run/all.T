# Args to vt are:
#	extra compile flags
#	extra run flags
#	expected process return value, if not zero

test('drvrun001', normal, compile_and_run, [''])
test('drvrun002', normal, compile_and_run, [''])
test('drvrun003', normal, compile_and_run, [''])
test('drvrun004', normal, compile_and_run, [''])
test('drvrun005', normal, compile_and_run, [''])
test('drvrun006', normal, compile_and_run, [''])
test('drvrun007', normal, compile_and_run, [''])
test('drvrun008', normal, compile_and_run, ['-funbox-strict-fields'])
test('drvrun009', normal, compile_and_run, [''])
test('drvrun010', normal, compile_and_run, [''])
test('drvrun011', normal, compile_and_run, [''])
test('drvrun012', normal, compile_and_run, [''])
test('drvrun013', normal, compile_and_run, [''])
test('drvrun014', normal, compile_and_run, [''])
test('drvrun015', normal, compile_and_run, [''])
test('drvrun016', normal, compile_and_run, ['-funbox-strict-fields'])
test('drvrun017', only_compiler_types(['ghc']), compile_and_run, [''])
test('drvrun018', normal, compile_and_run, [''])
