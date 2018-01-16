test('T8184', extra_files(['A.hs', 'B.hs', 'B.hs-boot', 'C.hs']), run_command,
     ['$MAKE -s --no-print-directory T8184'])
