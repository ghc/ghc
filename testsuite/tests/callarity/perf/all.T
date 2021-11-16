test('T3924',
     [collect_stats('bytes allocated',8)
     , javascript_failures
     , only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])

