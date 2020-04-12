test('prog001',
     [extra_files(['../shell.hs', 'A.hs', 'B.hs', 'C1.hs', 'D1.hs', 'D2.hs']),
      when(arch('arm'), fragile(17555)),
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      req_interp,
      unless(opsys('mingw32') or not config.have_RTS_linker, extra_ways(['ghci-ext']))],
     ghci_script, ['prog001.script'])
