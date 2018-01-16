test('T8602',
     [extra_files(['A.hs']),
      extra_clean(['t8602.sh']),
      # Windows runs the preprocessor using runInteractiveProcess and can't
      # properly run the generated shell script as a result, since it can't
      # recognize e.g. a shebang or anything.
      when(opsys('mingw32'), expect_fail)],
     ghci_script, ['T8602.script'])
