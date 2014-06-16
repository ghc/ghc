test('T8602',
     [extra_clean(['t8602.sh']),
      when(opsys('mingw32'), expect_fail)],
     ghci_script, ['T8602.script'])
