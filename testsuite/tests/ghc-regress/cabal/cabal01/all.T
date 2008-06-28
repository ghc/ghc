setTestOpts(only_compiler_types(['ghc']))

if config.have_profiling:
    prof = '--enable-library-profiling'
else:
    prof = ''

test('cabal01',
     normal,
     run_command, ['$MAKE -s --no-print-directory cabal01 PROF=' + prof])
if default_testopts.cleanup != '':
    runCmd('$MAKE -C ' + in_testdir('') + ' clean')
