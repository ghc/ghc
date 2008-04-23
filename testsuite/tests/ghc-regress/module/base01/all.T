setTestOpts(only_compiler_types(['ghc']))

test('base01', compose(normalise_slashes, if_compiler_lt('ghc', '6.9', skip)),
     run_command,
     ['$MAKE -s base01 --no-print-directory'])

if default_testopts.cleanup != '':
    runCmd('$MAKE -s -C ' + in_testdir('') + ' clean')

