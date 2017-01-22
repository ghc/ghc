test('prog001',
     [cmd_prefix('ghciWayFlags=' + config.ghci_way_flags),
      when(arch('powerpc64') or arch('powerpc64le'),
           expect_broken_for(11259, ['ghci-ext'])),
      unless(opsys('mingw32'), extra_ways(['ghci-ext']))],
     ghci_script, ['prog001.script'])
