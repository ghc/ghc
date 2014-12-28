# We only want to run these tests with GHCi
def just_ghci( name, opts ):
  opts.only_ways = ['ghci']

test('eval', normal, compile_and_run, [''])
test('match', normal, compile_and_run, [''])
test('ex-prov-run', normal, compile_and_run, [''])
test('bidir-explicit', normal, compile_and_run, [''])
test('bidir-explicit-scope', normal, compile_and_run, [''])
test('T9783', normal, compile_and_run, [''])
test('match-unboxed', normal, compile_and_run, [''])
test('unboxed-wrapper', normal, compile_and_run, [''])
test('ghci', just_ghci, ghci_script, ['ghci.script'])
