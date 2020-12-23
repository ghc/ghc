test('GHCiDRF'
  , [extra_hc_opts("-fimplicit-import-qualified")
    , extra_files(['GHCiDRF.hs'])
    , combined_output
    ], ghci_script, ['GHCiDRF.script'])
