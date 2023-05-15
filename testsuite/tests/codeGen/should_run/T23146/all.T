test('T23146', normal, compile_and_run, [''])
test('T23146_lifted', normal, compile_and_run, [''])
test('T23146_liftedeq', expect_broken_for(23060, ghci_ways), compile_and_run, [''])
test('T23146_lifted_unlifted', normal, compile_and_run, [''])
