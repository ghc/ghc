test('rn.prog006', [extra_files(['A.hs', 'B/', 'Main.hs', 'pwd.hs'])], run_command,
     ['$MAKE -s --no-print-directory rn.prog006'])
