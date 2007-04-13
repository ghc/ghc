esc = skip_unless_tag('esc')

test('TestList', esc, compile, ['-fesc'])
test('TestImport', esc, compile, ['-fesc'])
test('TestData', esc, compile, ['-fesc'])
