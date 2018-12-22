test('T16038',
     [extra_files(['A.hs', 'A.hs-boot', 'B.hs'])],
     run_command,
     ['$MAKE -s --no-print-directory T16038'])
