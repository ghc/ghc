test('T3007',
     [when(opsys('darwin'), expect_broken(20026)),
      extra_files(['A/', 'B/'])],
     makefile_test, [])
