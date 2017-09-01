test('T10858',
     [ collect_stats('bytes allocated',8,True),
      only_ways(['normal'])],
     compile,
     ['-O'])

