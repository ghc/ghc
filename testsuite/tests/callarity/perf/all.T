test('T3924',
     [collect_stats('bytes allocated',8)
     , only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

