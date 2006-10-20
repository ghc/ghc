setTestOpts(only_compiler_types(['ghc']))

test('mod175', normal, run_command, ['$MAKE -s mod175'])

if default_testopts.cleanup != '':
    runCmd('$MAKE -C ' + in_testdir('') + ' clean')

