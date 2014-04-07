test('T3924',
     [stats_num_field('bytes allocated', 
          [ (wordsize(64), 51480, 5),
              # previously, without call-arity: 22326544
              # 2014-01-18: 51480  (amd64/Linux)
            (wordsize(32), 44988, 5) ]),
              # 2014-04-04: 44988  (Windows, 64-bit machine)
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])
