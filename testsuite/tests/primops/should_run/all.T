test('T6135', normal, compile_and_run, [''])
test('T7689', normal, compile_and_run, [''])
# The test is using unboxed tuples, so omit ghci
test('T9430', omit_ways(['ghci']), compile_and_run, [''])
