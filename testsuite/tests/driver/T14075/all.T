test('T14075',
     [ extra_files(['F.hs', 'F.hs-boot', 'O.hs', 'V.hs', 'V.hs-boot'])
     , req_smp, # uses ghc --make -j2
     ],
     run_command,
     ['$MAKE -s --no-print-directory T14075'])
