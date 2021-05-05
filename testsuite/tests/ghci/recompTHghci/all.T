test('recompTHghci',
     [extra_files(['../shell.hs', 'A.hs', 'B1.hs', 'B2.hs']),
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      req_interp,
      unless(opsys('mingw32') or not config.have_RTS_linker, extra_ways(['ghci-ext']))],
     ghci_script, ['recompTHghci.script'])
