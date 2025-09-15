test('prog019',
     [extra_hc_opts('-fkeep-going'),
      extra_files(['A.hs', 'B.hs', 'B1.hs', 'B2.hs', 'C.hs', 'D.hs', 'E.hs'])],
     ghci_script, ['prog019.script'])
