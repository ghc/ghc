# This test verifies that fixity is correctly reflected in Generic instances
# derived for types living outside the current module.

test('T18101', expect_broken(18101), ghci_script, ['T18101.script'])
