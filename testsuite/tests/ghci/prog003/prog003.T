# This test is flaky when building statically, in particular
# on Windows.
# It is a genuine bug that should be looked at, but
# for the sake of stability of the build bot we disable it for now.
# See #11317, #20704.
test('prog003',
     [extra_files(['A.hs', 'B.hs', 'C.hs', 'D1.hs', 'D2.hs']),
      unless(config.ghc_dynamic, fragile(20704)),
      cmd_prefix('ghciWayFlags=' + config.ghci_way_flags)],
     ghci_script, ['prog003.script'])
