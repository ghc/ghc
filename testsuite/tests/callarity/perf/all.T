test('T3924',
     [comparison('bytes allocated',8)
     , only_ways(['normal'])
      ],
     compile_and_run,
     ['-O'])
