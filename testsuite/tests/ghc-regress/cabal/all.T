setTestOpts(only_compiler_types(['ghc']))

test('ghcpkg01', normal, run_command_ignore_output, ['$MAKE ghcpkg01'])
test('ghcpkg02', normal, run_command_ignore_output, ['$MAKE ghcpkg02'])
test('ghcpkg03', normal, run_command_ignore_output, ['$MAKE ghcpkg03'])
