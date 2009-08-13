test('T3007', normal,
     run_command, ['$MAKE -s --no-print-directory T3007'])
if default_testopts.cleanup != '':
    runCmd('$MAKE -C ' + in_testdir('') + ' clean')
