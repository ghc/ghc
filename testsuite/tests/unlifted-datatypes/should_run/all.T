test('UnlData1', normal, compile_and_run, [''])
test('UnlGadt1', [exit_code(1), expect_broken_for(23060, ghci_ways)], compile_and_run, [''])
test('T23549', normal, multimod_compile_and_run, ['T23549', ''])
test('TopLevel', normal, multimod_compile_and_run, ['TopLevel', '-O'])
