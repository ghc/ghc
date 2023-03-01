test('UnlData1', normal, compile_and_run, [''])
test('UnlGadt1', [exit_code(1), expect_broken_for(23060, ghci_ways)], compile_and_run, [''])
