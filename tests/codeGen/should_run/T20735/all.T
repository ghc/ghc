test('T20735',
     [req_c, omit_ghci],
     multi_compile_and_run,
     ['T20735', [('T20735_c.c', '')], ''])

