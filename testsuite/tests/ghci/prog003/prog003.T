# This test is very flaky on Windows.
# It is a genuine bug that should be looked at, but
# for the sake of stability of the build bot we disable it for now.
# See #11317.
test('prog003',
     [extra_files(['../shell.hs', 'A.hs', 'B.hs', 'C.hs', 'D1.hs', 'D2.hs']),
      when(opsys('mingw32'), skip),
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags)],
     ghci_script, ['prog003.script'])
