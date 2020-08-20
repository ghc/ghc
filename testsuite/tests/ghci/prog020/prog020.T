test('prog020a',
     [extra_files(['a/', 'b/'])],
     ghci_script, ['prog020a.script'])
test('prog020b',
     [extra_files(['a/', 'b/'])],
     ghci_script, ['prog020b.script'])
test('prog020c',
     [extra_files(['b/', 'c/'])],
     ghci_script, ['prog020c.script'])
test('prog020d',
     [extra_files(['a/', 'b/', 'c/'])],
     ghci_script, ['prog020d.script'])
