test('T16893',
     [expect_broken(16893), extra_files(['Complex.hs'])],
     compile_and_run,
     ['-O1'])
