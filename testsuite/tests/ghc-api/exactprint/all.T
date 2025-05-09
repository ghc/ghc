setTestOpts(req_hadrian_deps(['test:check-exact']))

test('RenameCase1',   ignore_stderr, makefile_test, ['RenameCase1'])
test('LayoutLet2',    ignore_stderr, makefile_test, ['LayoutLet2'])
test('LayoutLet3',    ignore_stderr, makefile_test, ['LayoutLet3'])
test('LayoutLet4',    ignore_stderr, makefile_test, ['LayoutLet4'])
test('Rename1',       ignore_stderr, makefile_test, ['Rename1'])
test('Rename2',       ignore_stderr, makefile_test, ['Rename2'])
test('LayoutIn1',     ignore_stderr, makefile_test, ['LayoutIn1'])
test('LayoutIn3',     ignore_stderr, makefile_test, ['LayoutIn3'])
test('LayoutIn3a',    ignore_stderr, makefile_test, ['LayoutIn3a'])
test('LayoutIn3b',    ignore_stderr, makefile_test, ['LayoutIn3b'])
test('LayoutIn4',     ignore_stderr, makefile_test, ['LayoutIn4'])
test('LocToName',     ignore_stderr, makefile_test, ['LocToName'])
test('LetIn1',        ignore_stderr, makefile_test, ['LetIn1'])
test('WhereIn4',      ignore_stderr, makefile_test, ['WhereIn4'])
test('AddDecl1',      ignore_stderr, makefile_test, ['AddDecl1'])
test('AddDecl2',      ignore_stderr, makefile_test, ['AddDecl2'])
test('AddDecl3',      ignore_stderr, makefile_test, ['AddDecl3'])
test('LocalDecls',    ignore_stderr, makefile_test, ['LocalDecls'])
test('LocalDecls2',   ignore_stderr, makefile_test, ['LocalDecls2'])
test('WhereIn3a',     ignore_stderr, makefile_test, ['WhereIn3a'])
test('WhereIn3b',     ignore_stderr, makefile_test, ['WhereIn3b'])
test('AddLocalDecl1', ignore_stderr, makefile_test, ['AddLocalDecl1'])
test('AddLocalDecl2', ignore_stderr, makefile_test, ['AddLocalDecl2'])
test('AddLocalDecl3', ignore_stderr, makefile_test, ['AddLocalDecl3'])
test('AddLocalDecl4', ignore_stderr, makefile_test, ['AddLocalDecl4'])
test('AddLocalDecl5', ignore_stderr, makefile_test, ['AddLocalDecl5'])
test('AddLocalDecl6', ignore_stderr, makefile_test, ['AddLocalDecl6'])
test('RmDecl1',       ignore_stderr, makefile_test, ['RmDecl1'])
test('RmDecl2',       ignore_stderr, makefile_test, ['RmDecl2'])
test('RmDecl3',       ignore_stderr, makefile_test, ['RmDecl3'])
test('RmDecl4',       ignore_stderr, makefile_test, ['RmDecl4'])
test('RmDecl5',       ignore_stderr, makefile_test, ['RmDecl5'])
test('RmDecl6',       ignore_stderr, makefile_test, ['RmDecl6'])
test('RmDecl7',       ignore_stderr, makefile_test, ['RmDecl7'])
test('RmTypeSig1',    ignore_stderr, makefile_test, ['RmTypeSig1'])
test('RmTypeSig2',    ignore_stderr, makefile_test, ['RmTypeSig2'])
test('AddHiding1',    ignore_stderr, makefile_test, ['AddHiding1'])
test('AddHiding2',    ignore_stderr, makefile_test, ['AddHiding2'])
test('AddClassMethod',ignore_stderr, makefile_test, ['AddClassMethod'])
test('Test20239',  normal, compile_fail, ['-dsuppress-uniques -ddump-parsed-ast -dkeep-comments'])
test('ZeroWidthSemi',  normal, compile, ['-dsuppress-uniques -ddump-parsed-ast -dkeep-comments'])
test('T22919',  normal, compile, ['-dsuppress-uniques -ddump-parsed-ast -dkeep-comments'])
