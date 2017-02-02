test('T10858',
     [compiler_stats_num_field('bytes allocated',
          [ (wordsize(64), 304094944, 8) ]),
          # Initial:    476296112
          # 2016-12-19  247768192  Join points (#19288)
          # 2016-02-12  304094944  Type-indexed Typeable
      only_ways(['normal'])],
     compile,
     ['-O'])
