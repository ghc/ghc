test('T16392',
     [extra_files(['A.hs']),
      when(config.have_RTS_linker,extra_ways(['ghci-ext'])),
      req_interp,
      when(opsys('mingw32'), fragile(22649))
      ],
     ghci_script, ['T16392.script'])
