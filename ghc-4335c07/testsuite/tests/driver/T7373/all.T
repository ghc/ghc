test('T7373', [extra_files(['D.hs', 'pkg/']),
               expect_broken(7373)], run_command,
     ['$MAKE -s --no-print-directory T7373'])
