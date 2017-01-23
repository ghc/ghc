test('T10858',
     [compiler_stats_num_field('bytes allocated',
          [ (wordsize(64), 222312440, 8) ]),
      only_ways(['normal'])],
     compile,
     ['-O'])
