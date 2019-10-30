test('T16392',
     [extra_files(['A.hs']),
      extra_ways(['ghci-ext']),
      unless(config.have_RTS_linker, skip)],
     ghci_script, ['T16392.script'])
