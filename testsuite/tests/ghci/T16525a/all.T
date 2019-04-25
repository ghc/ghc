test('T16525a',
     [extra_files(['A.hs', 'B.hs', ]),
      extra_run_opts('+RTS -DS -RTS'),
      when(ghc_dynamic(), skip), ],
     ghci_script, ['T16525a.script'])
