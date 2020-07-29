test('T16392',
     [extra_files(['A.hs']),
      extra_ways(['ghci-ext'])],
     ghci_script, ['T16392.script'])
