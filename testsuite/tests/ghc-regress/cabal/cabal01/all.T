setTestOpts(only_compiler_types(['ghc']))

test('cabal01', ignore_output, run_command, ['$MAKE cabal01'])

if default_testopts.cleanup != '':
    runCmd('$MAKE -C ' + in_testdir('') + ' clean')
