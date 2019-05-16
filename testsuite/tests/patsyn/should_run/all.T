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
# we omit profasm/profthreaded because it doesn't bring much to the table but
# introduces its share of complexity, as the test as it is fails with
# profasm:
# T13688.hs:6:13: fatal:
#     Cannot load -prof objects when GHC is built with -dynamic
#     To fix this, either:
#       (1) Use -fexternal-interpreter, or
#       (2) Build the program twice: once with -dynamic, and then
#           with -prof using -osuf to set a different object file suffix.
test('T13688', omit_ways(['profasm', 'profthreaded']), multimod_compile_and_run, ['T13688', '-v0'])
# Requires UnboxedSums, which GHCi does not support.
test('T14228', omit_ways(['ghci']), compile_and_run, [''])
