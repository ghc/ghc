test('recomp005',
     compose(if_compiler_lt('ghc', '6.9', skip), skip_if_fast),
     run_command, ['$MAKE -s --no-print-directory recomp005'])

if default_testopts.cleanup != '':
    runCmd('$MAKE -C ' + in_testdir('') + ' clean')
