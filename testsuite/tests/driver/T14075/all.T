test('T14075',
     [ extra_files(['F.hs', 'F.hs-boot', 'O.hs', 'V.hs', 'V.hs-boot'])
     , req_smp, # uses ghc --make -j2
     ],
     makefile_test, [])
