test('1372', if_compiler_lt('ghc', '6.9', skip),
     run_command, ['$MAKE -s --no-print-directory 1372'])
if default_testopts.cleanup != '':
    runCmd('$MAKE -C ' + in_testdir('') + ' clean')
