test('T23612', only_ways(['ghci-opt']), ghci_script, ['T23612.script'])
test('T23612b', [only_ways(['ghci-opt']), extra_files(['T23612bA.hs', 'T23612bB.hs'])], ghci_script, ['T23612b.script'])
