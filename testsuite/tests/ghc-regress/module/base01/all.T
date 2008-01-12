setTestOpts(only_compiler_types(['ghc']))

test('base01', if_compiler_ge('ghc', '6.9', expect_broken(2034)),
               run_command, ['$MAKE -s base01 --no-print-directory'])

if default_testopts.cleanup != '':
    runCmd('$MAKE -s -C ' + in_testdir('') + ' clean')

