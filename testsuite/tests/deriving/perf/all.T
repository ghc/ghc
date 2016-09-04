test('T10858',
     [compiler_stats_num_field('bytes allocated',
          [ (wordsize(64), 241655120, 8) ]),
      only_ways(['normal'])
      ],
     compile,
     ['-O'])
