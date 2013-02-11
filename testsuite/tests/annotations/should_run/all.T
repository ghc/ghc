setTestOpts(when(compiler_profiled(), skip))
# These tests are very slow due to their use of package GHC
setTestOpts(skip_if_fast)

# Annotations, like Template Haskell, require runtime evaluation.  In
# order for this to work with profiling, we would have to build the
# program twice and use -osuf p_o (see the TH_splitE5_prof test).  For
# now, just disable the profiling and dynamic ways.
test('annrun01',
     [extra_clean(['Annrun01_Help.hi', 'Annrun01_Help.o',
                    'annrun01.hi', 'annrun01.o',
                    'Config.hs', 'Config.hi', 'Config.o']),
      pre_cmd('$MAKE -s --no-print-directory config'),
      omit_ways(['profasm','profthreaded', 'dyn'])],
      multimod_compile_and_run,
      ['annrun01', '-package ghc']
    )

""""
Helpful things to C+P:

test('', normal, compile_fail, [''])

test('', normal, compile, [''])

test('', extra_clean(['.hi', '.o']), 
     multimod_compile_fail, ['', '-v0'])

test('', extra_clean(['.hi', '.o']), 
     multimod_compile, ['', '-v0'])
"""
