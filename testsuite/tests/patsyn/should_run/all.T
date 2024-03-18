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
test('records-run', normal, compile_and_run, [''])
test('ghci', just_ghci, ghci_script, ['ghci.script'])
test('T11985', just_ghci, ghci_script, ['T11985.script'])
test('T11224', normal, compile_and_run, ['-Wincomplete-patterns -Woverlapping-patterns'])
test('T13688', req_th, multimod_compile_and_run, ['T13688', '-v0'])
test('T14228', normal, compile_and_run, [''])
test('records-poly-update', normal, compile_and_run, [''])
test('T24552', normal, compile_and_run, [''])