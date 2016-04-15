test('T11827',
     [extra_clean(['A.hi', 'A.hi-boot', 'A.o', 'B.hi', 'B.o']),
      exit_code(1), expect_broken(11827)],
     ghci_script, ['T11827.script'])
