test('T11827', [extra_files(['A.hs', 'A.hs-boot', 'B.hs']),
                expect_broken(11827)], ghci_script, ['T11827.script'])
