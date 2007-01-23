test('dynamic_flags_001', skip_if_fast, run_command, ['$MAKE -s --no-print-directory dynamic_flags_001'])

if default_testopts.cleanup != '':
    runCmd('$MAKE -s -C ' + in_testdir('') + ' clean')
