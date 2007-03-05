setTestOpts(only_compiler_types(['ghc']))

test('cabal01', compose(ignore_output, expect_broken_if_platform(1196, 'i386-unknown-mingw32')), run_command, ['$MAKE cabal01'])

if default_testopts.cleanup != '':
    runCmd('$MAKE -C ' + in_testdir('') + ' clean')
