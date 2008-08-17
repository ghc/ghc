setTestOpts(only_compiler_types(['ghc']))

test('base01', normalise_slashes,
     run_command,
     ['$MAKE -s base01 --no-print-directory'])

if default_testopts.cleanup != '':
    runCmd('$MAKE -s -C ' + in_testdir('') + ' clean')

