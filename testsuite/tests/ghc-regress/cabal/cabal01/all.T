setTestOpts(only_compiler_types(['ghc']))

test('cabal01', normal, run_command_ignore_output, ['$MAKE cabal01'])
