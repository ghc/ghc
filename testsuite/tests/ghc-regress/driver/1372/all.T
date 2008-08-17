test('1372', normal,
     run_command, ['$MAKE -s --no-print-directory 1372'])
if default_testopts.cleanup != '':
    runCmd('$MAKE -C ' + in_testdir('') + ' clean')
