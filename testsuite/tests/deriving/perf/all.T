test('T10858',
     [ comparison('bytes allocated',8,True),
      only_ways(['normal'])],
     compile,
     ['-O'])
