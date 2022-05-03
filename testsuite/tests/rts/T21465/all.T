# Check that the full width of callee-saved XMM registers are preserved across
# calls into Haskell on Windows (although we allow this test to run on
# non-Linux platforms as well).

test('T21465', when(not arch('x86_64'), skip), compile_and_run, ['T21465_c.c'])
