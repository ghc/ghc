test('T11827', [extra_files(['A.hs', 'A.hs-boot', 'B.hs']),
               ], ghci_script, ['T11827.script'])
