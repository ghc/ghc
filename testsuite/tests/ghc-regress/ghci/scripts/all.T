test('ghci001', normal, ghci_script, ['ghci001.script'])
test('ghci002', normal, ghci_script, ['ghci002.script'])
test('ghci003', normal, ghci_script, ['ghci003.script'])
test('ghci004', normal, ghci_script, ['ghci004.script'])
test('ghci005', normal, ghci_script, ['ghci005.script'])
test('ghci006', normal, ghci_script, ['ghci006.script'])
test('ghci007', normal, ghci_script, ['ghci007.script'])
test('ghci008', normal, ghci_script, ['ghci008.script'])
test('ghci009', normal, ghci_script, ['ghci009.script'])

# Skip this test: deadlock can't be detected now, because we wait for
# signals to arrive if there are signal handlers installed, and GHCi
# has a ^C handler installed.
test('ghci010', skip, ghci_script, ['ghci010.script'])

# Expect failure: this test should list both the type constructor and the data
# constructor for each of the builtin identifiers ([], (), (,), etc.), but
# currently only gives the data constructor.
test('ghci011', expect_fail, ghci_script, ['ghci011.script'])
