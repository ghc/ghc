test('T16392',
     [extra_files(['A.hs']),
      skip,
      extra_ways(['ghci-ext']),
      req_rts_linker],
     ghci_script, ['T16392.script'])
