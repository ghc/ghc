test('T16511',
    [extra_files(['B1.hs', 'B2.hs', 'D.hs', 'A.hs', 'C.hs']),
     when(opsys('mingw32'), expect_broken(17034))
    ],
    ghci_script,
    ['T16511.script'])
