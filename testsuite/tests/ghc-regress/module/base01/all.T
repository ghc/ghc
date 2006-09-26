setTestOpts(only_compiler_types(['ghc']))

test('base01', normal, run_command, ['$MAKE -s base01'])

if default_testopts.cleanup != '':
    runCmd('$MAKE -s -C ' + in_testdir('') + ' clean')

