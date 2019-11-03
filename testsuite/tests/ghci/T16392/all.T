test('T16392',
     [extra_files(['A.hs']),
      when(config.have_ext_interp, extra_ways(['ghci-ext']))],
     ghci_script, ['T16392.script'])
