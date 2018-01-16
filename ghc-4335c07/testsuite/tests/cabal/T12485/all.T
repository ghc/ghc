test('T12485', [extra_files(['a.pkg', 'b.pkg', 'Main.hs'])], run_command, ['$MAKE -s --no-print-directory T12485'])
