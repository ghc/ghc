test('T27370',
     [extra_files(['T27370.hs', 'T27370.pp']),
      when(opsys('mingw32'), skip)],
     makefile_test, ['T27370'])
