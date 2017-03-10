test('linkwhole', [extra_files(['Types.hs','Main.hs','MyCode.hs','Handles.hs']),
                   when(opsys('mingw32'), skip)],
     run_command, ['$MAKE -s --no-print-directory linkwhole'])
