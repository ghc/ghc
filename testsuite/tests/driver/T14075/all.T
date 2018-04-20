test('T14075',
     [extra_files(['F.hs', 'F.hs-boot', 'O.hs', 'V.hs', 'V.hs-boot'])],
     run_command,
     ['$MAKE -s --no-print-directory T14075'])
