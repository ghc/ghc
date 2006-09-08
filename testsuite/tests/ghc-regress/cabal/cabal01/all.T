setTestOpts(only_compiler_types(['ghc']))

test('cabal01', normal, run_command_ignore_output, ['$MAKE cabal01'])

if default_testopts.cleanup != '':
    runCmd('$MAKE -C ' + in_testdir('') + ' clean')
