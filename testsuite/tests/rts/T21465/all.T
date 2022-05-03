# Check that the full width of callee-saved XMM registers are preserved across
# calls into Haskell on Windows.

test('T21465', unless(opsys('mingw32') and arch('x86_64'), skip), compile_and_run, ['T21465_c.c'])
