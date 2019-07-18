test('T16893',
     [exit_code(1), extra_files(['Complex.hs'])],
     compile_and_run,
     ['-O1'])
