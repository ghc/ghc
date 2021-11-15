# testcase for bug #936
test('ghci.prog008', [extra_files(['A.hs']),
	skip # ghcjs
	], ghci_script, ['ghci.prog008.script'])
