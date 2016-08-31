test('T12485',
     [extra_clean(['a.db', 'b.db', 'Main.o', 'Main', 'Main.hi']),
      expect_broken(12485)],
     run_command,
     ['$MAKE -s --no-print-directory T12485'])
