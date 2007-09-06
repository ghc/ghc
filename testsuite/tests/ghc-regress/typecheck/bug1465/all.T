test('bug1465', normal, run_command, ['$MAKE -s --no-print-directory bug1465'])
if default_testopts.cleanup != '':
    runCmd('$MAKE -C ' + in_testdir('') + ' clean')
