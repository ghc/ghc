test('T16525a',
     [skip, extra_files(['A.hs', 'B.hs'])],
     ghci_script, ['T16525a.script'])
