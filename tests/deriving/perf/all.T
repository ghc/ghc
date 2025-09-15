test('T10858',
     [ collect_compiler_stats('bytes allocated',8),
      only_ways(['normal'])],
     compile,
     ['-O'])

