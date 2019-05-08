test('T16511', [extra_files(['B1.hs', 'B2.hs', 'D.hs', 'A.hs', 'C.hs']), ],
               ghci_script, ['T16511.script'])
