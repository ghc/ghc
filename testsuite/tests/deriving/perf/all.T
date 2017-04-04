test('T10858',
     [compiler_stats_num_field('bytes allocated',
          [(wordsize(64), 241242968, 8) ]),
          # Initial:    222312440
          # 2016-12-19  247768192  Join points (#19288)
          # 2017-02-12  304094944  Type-indexed Typeable
          # 2017-02-25  275357824  Early inline patch
          # 2017-03-28  241242968  Run Core Lint less
      only_ways(['normal'])],
     compile,
     ['-O'])
