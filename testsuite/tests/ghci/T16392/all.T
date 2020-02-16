test('T16392',
     [extra_files(['A.hs']),
      when(config.have_RTS_linker,extra_ways(['ghci-ext'])),
      req_interp],
     ghci_script, ['T16392.script'])
