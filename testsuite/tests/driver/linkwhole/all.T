test('linkwhole',
     [extra_files(['Types.hs','Main.hs','MyCode.hs','Handles.hs']),
      req_rts_linker,
      when(opsys('freebsd'), expect_broken(16035)),
      when(opsys('mingw32'), skip)],
     makefile_test, [])
