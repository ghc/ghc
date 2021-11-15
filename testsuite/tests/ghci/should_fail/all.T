#ghcjs skip

test('T10549', [skip], ghci_script, ['T10549.script'])
test('T10549a', [skip], ghci_script, ['T10549a.script'])
test('T15055', [skip, normalise_version('ghc')], ghci_script, ['T15055.script'])
test('T16013', [skip], ghci_script, ['T16013.script'])
test('T16287', [skip], ghci_script, ['T16287.script'])
