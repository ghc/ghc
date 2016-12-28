# This test is very flaky on Windows.
# It is a genuine bug that should be looked at, but
# for the sake of stability of the build bot we disable it for now.
# See Trac 11317.
test('prog003',
     [extra_clean(['D.hs', 'D.hi', 'C.hi', 'C.o', 'B.hi', 'B.o',
                   'A', 'A.hi', 'A.o', 'a.out']),
      when(opsys('mingw32'), skip),
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags)],
     ghci_script,
     ['prog003.script'])

