setTestOpts(only_compiler_types(['ghc']))

test('ghciprog004', normal, run_command_ignore_output, ['$MAKE ghciprog004'])
clean(['ctest.c', 'ctest.o'])
