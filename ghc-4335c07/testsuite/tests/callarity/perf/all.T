test('T3924',
     [stats_num_field('bytes allocated', 
          [ (wordsize(64), 50760, 8),
              # previously, without call-arity: 22326544
              # 2014-01-18: 51480  (amd64/Linux)
              # 2014-07-17: 50760  (amd64/Linux) (Roundabout adjustment)
              # 2015-04-03: Widen 5->8% (amd64/Windows was doing better)
            (wordsize(32), 44988, 5) ]),
              # 2014-04-04: 44988  (Windows, 64-bit machine)
      only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])
