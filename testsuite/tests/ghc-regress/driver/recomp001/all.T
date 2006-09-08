test('recomp001', skip_if_fast, run_command, ['$MAKE -s --no-print-directory recomp001'])

if default_testopts.cleanup != '':
    runCmd('$MAKE -C ' + in_testdir('') + ' clean')
