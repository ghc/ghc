test('caf_crash',
     [extra_files(['A.hs', 'B.hs', 'D.hs', ]),
      when(ghc_dynamic(), skip),
      extra_ways(['ghci-ext']),
      omit_ways(['ghci']),
      js_broken(22359)],
     ghci_script, ['caf_crash.script'])
