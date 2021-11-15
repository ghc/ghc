test('apirecomp001',
     [extra_files(['A.hs', 'B.hs', 'myghc.hs']),
      when(opsys('OSJavaScript'), skip)],
     makefile_test, ['apirecomp001'])
