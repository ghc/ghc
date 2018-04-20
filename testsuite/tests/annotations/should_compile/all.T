# Annotations, like Template Haskell, require runtime evaluation.  In
# order for this to work with profiling, we would have to build the
# program twice and use -osuf p_o (see the TH_spliceE5_prof test).  For
# now, just disable the profiling ways.
test('ann01', [req_interp, omit_ways(prof_ways)], compile, ['-v0'])
test('T14129', [req_interp, omit_ways(prof_ways)], compile, ['-v0'])

""""
Helpful things to C+P:

test('', normal, compile_fail, [''])

test('', normal, compile, [''])

test('', [], multimod_compile_fail, ['', '-v0'])

test('', [], multimod_compile, ['', '-v0'])
"""
