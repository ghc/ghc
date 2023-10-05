test('caf_crash',
     [extra_files(['A.hs', 'B.hs', 'D.hs', ]),
      when(ghc_dynamic(), skip),
      extra_ways(['ghci-ext']),
      js_broken(22359)],
     ghci_script, ['caf_crash.script'])
