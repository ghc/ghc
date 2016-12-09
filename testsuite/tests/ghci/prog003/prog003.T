test('prog003',
     [extra_clean(['D.hs', 'D.hi', 'C.hi', 'C.o', 'B.hi', 'B.o',
                   'A', 'A.hi', 'A.o', 'a.out']),
      when(opsys('mingw32'), expect_broken(11317)),
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags)],
     ghci_script,
     ['prog003.script'])

