test('T16893',
     [extra_files(['Complex.hs']), exit_code(1)],
     compile_and_run,
     ['-O1'])
