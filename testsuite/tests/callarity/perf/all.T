test('T3924',
     [stats_num_field('bytes allocated', (51480, 5)),
          # previously, without call-arity: 22326544
          # 2014-01-18: 51480  (amd64/Linux)
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])
