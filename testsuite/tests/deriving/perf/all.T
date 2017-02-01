test('T10858',
     [compiler_stats_num_field('bytes allocated',
          [ (wordsize(64),  247768192, 8) ]),
          # Initial:    222312440
          # 2016-12-19  247768192  Join points (#19288)
      only_ways(['normal'])],
     compile,
     ['-O'])
