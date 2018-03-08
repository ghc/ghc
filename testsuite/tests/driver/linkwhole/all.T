test('linkwhole',
     [extra_files(['Types.hs','Main.hs','MyCode.hs','Handles.hs']),
      when(arch('powerpc64') or arch('powerpc64le'), expect_broken(11259)),
      when(opsys('mingw32'), skip)],
     run_command, ['$MAKE -s --no-print-directory linkwhole'])
