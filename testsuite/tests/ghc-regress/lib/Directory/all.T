test('currentDirectory001',     normal, compile_and_run, [''])
test('directory001',            normal, compile_and_run, [''])
test('doesDirectoryExist001',   normal, compile_and_run, [''])

# This test is a bit bogus.  Disable for GHCi.
test('getDirContents001', omit_ways(['ghci']), compile_and_run, ['-fno-gen-manifest'])

# Depends on binary from previous run, which gets removed by the driver way=ghci
test('getPermissions001', omit_ways(['ghci']), compile_and_run, ['-cpp'])
