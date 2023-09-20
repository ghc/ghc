test('sigcabal01',
     [extra_files(['Main.hs', 'Setup.hs', 'p/']), expect_broken(10622)],
     makefile_test,
     [])
