test('retc001',
     [extra_files(['A.hs', 'B1.hs', 'B2.hs', 'C.hs']),
# See issue 11204, this test sometimes passes and sometimes fails on OSX
      when(opsys('darwin'), skip)],
     makefile_test, [])

