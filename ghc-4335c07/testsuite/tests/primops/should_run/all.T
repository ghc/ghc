test('T6135', normal, compile_and_run, [''])
test('T7689', normal, compile_and_run, [''])
# The test is using unboxed tuples, so omit ghci
test('T9430', omit_ways(['ghci']), compile_and_run, [''])
test('T10481', exit_code(1), compile_and_run, [''])
test('T10678',
     [stats_num_field('bytes allocated',
                      [(wordsize(64), 64004171, 5)
                       # 2015-11-04: 88041768 +/- 5%  (before runRW#)
                       # 2015-11-04: 64004171         (after runRW#)
                      ]),
      only_ways('normal')
     ],
     compile_and_run, ['-O'])
test('T11296', normal, compile_and_run, [''])
test('T13825-compile', normal, compile_and_run, [''])
