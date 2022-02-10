test('T3924',
     [collect_stats('bytes allocated',8)
     , only_ways(['normal'])
     , js_broken(22261)
     ],
     compile_and_run,
     ['-O'])

