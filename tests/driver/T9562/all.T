test('T9562', [extra_files(['A.hs', 'B.hs', 'B.hs-boot', 'C.hs', 'D.hs', 'Main.hs']),
               expect_broken(9562)],
     makefile_test, [])
