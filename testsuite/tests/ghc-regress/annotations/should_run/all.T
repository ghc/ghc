# Annotations were implemented in GHC 6.11 and not supported in other compilers
# These tests are very slow due to their use of package GHC
setTestOpts(compose(skip_if_fast, if_compiler_lt('ghc', '6.11', skip)));

runCmd('$MAKE -C ' + in_testdir('') + ' config')

# Annotations, like Template Haskell, require runtime evaluation.  In
# order for this to work with profiling, we would have to build the
# program twice and use -osuf p_o (see the TH_splitE5_prof test).  For
# now, just disable the profiling ways.
test('annrun01',
     [extra_clean(['Annrun01_Help.hi', 'Annrun01_Help.o',
                    'annrun01.hi', 'annrun01.o']),
      omit_ways(['profc','profasm','profthreaded'])],
      multimod_compile_and_run,
      ['annrun01', '-package ghc']
    )

if default_testopts.cleanup != '':
    runCmd('$MAKE -C ' + in_testdir('') + ' clean')

""""
Helpful things to C+P:

test('', normal, compile_fail, [''])

test('', normal, compile, [''])

test('', extra_clean(['.hi', '.o']), 
     multimod_compile_fail, ['', '-v0'])

test('', extra_clean(['.hi', '.o']), 
     multimod_compile, ['', '-v0'])
"""
