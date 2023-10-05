test('T20509',
     [extra_files(['p', 'q', 'T20509.cabal', 'Setup.hs'])
     , run_timeout_multiplier(2)
     , js_fragile(24259)
     ],
     makefile_test,
     [])
