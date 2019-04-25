test('T16525a',
     [extra_files(['A.hs', 'B.hs', ]),
      when(ghc_dynamic(), skip),
      extra_ways(['ghci-ext-prof']),
      omit_ways(['ghci']), ],
     ghci_script, ['T16525a.script'])
