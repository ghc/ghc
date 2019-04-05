test('dynamicToo006', [normalise_slashes, extra_files(['Main.hs'])],
     run_command, ['$MAKE -s main --no-print-director'])
