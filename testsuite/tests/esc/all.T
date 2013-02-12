esc = unless(tag('esc'), skip)

test('TestList', esc, compile, ['-fesc'])
test('TestImport', esc, compile, ['-fesc'])
test('TestData', esc, compile, ['-fesc'])
