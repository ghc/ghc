
setTestOpts(when(compiler_profiled(), skip))

# Annotations, like Template Haskell, require runtime evaluation.  In
# order for this to work with profiling, we would have to build the
# program twice and use -osuf p_o (see the TH_splitE5_prof test).  For
# now, just disable the profiling ways.
test('ann01',
     [req_interp, omit_ways(['profasm','profthreaded'])],
     compile,
     ['-v0'])

""""
Helpful things to C+P:

test('', normal, compile_fail, [''])

test('', normal, compile, [''])

test('', extra_clean(['.hi', '.o']), 
     multimod_compile_fail, ['', '-v0'])

test('', extra_clean(['.hi', '.o']), 
     multimod_compile, ['', '-v0'])
"""
