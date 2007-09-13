setTestOpts(only_compiler_types(['ghc']))

if config.have_profiling:
    prof = '--enable-library-profiling'
else:
    prof = ''

test('cabal01',
     ignore_output,
     run_command, ['$MAKE cabal01 PROF=' + prof])
if default_testopts.cleanup != '':
    runCmd('$MAKE -C ' + in_testdir('') + ' clean')
