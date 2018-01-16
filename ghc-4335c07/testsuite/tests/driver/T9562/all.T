test('T9562', [extra_files(['A.hs', 'B.hs', 'B.hs-boot', 'C.hs', 'D.hs', 'Main.hs']),
               expect_broken(9562)], run_command,
     ['$MAKE -s --no-print-directory T9562'])
