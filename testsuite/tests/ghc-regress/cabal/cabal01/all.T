setTestOpts(only_compiler_types(['ghc']))

test('cabal01',
     ignore_output,
     run_command, ['$MAKE cabal01 PROF='])
if default_testopts.cleanup != '':
    runCmd('$MAKE -C ' + in_testdir('') + ' clean')

test('cabal01prof',
     composes([req_profiling,
               ignore_output]),
     run_command, ['$MAKE cabal01 PROF=--enable-library-profiling'])

if default_testopts.cleanup != '':
    runCmd('$MAKE -C ' + in_testdir('') + ' clean')
