test('T6135', normal, compile_and_run, [''])
test('T7689', normal, compile_and_run, [''])
# The test is using unboxed tuples, so omit ghci
test('T9430', omit_ways(['ghci']), compile_and_run, [''])
test('T10481', exit_code(1), compile_and_run, [''])
test('T10678',
     [ comparison('bytes allocated',5),
      only_ways('normal')
     ],
     compile_and_run, ['-O'])
test('T11296', normal, compile_and_run, [''])
