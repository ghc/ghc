test('linkwhole',
     [extra_files(['Types.hs','Main.hs','MyCode.hs','Handles.hs']),
      unless(config.have_RTS_linker, skip),
      when(opsys('freebsd'), expect_broken(16035)),
      when(opsys('mingw32'), skip)],
     makefile_test, [])
