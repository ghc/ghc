test('currentDirectory001',     normal, compile_and_run, [''])
test('directory001',            normal, compile_and_run, [''])

# This test is a bit bogus.  Disable for GHCi.
test('getDirContents001', omit_ways(['ghci']), compile_and_run, [''])

test('getPermissions001',       normal, compile_and_run, ['-cpp'])
