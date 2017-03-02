test('linkwhole', [extra_files(['Types.hs','Main.hs','MyCode.hs','Handles.hs'])],
     run_command, ['$MAKE -s --no-print-directory linkwhole'])
