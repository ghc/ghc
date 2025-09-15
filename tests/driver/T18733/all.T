srcs = ['Library1.hs', 'Library2.hs', 'Main.hs']
test('T18733', extra_files(srcs), makefile_test, [])
