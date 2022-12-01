test('sum_mod', [normalise_slashes, extra_files(['Lib.hs', 'Main.hs'])],
     run_command, ['$MAKE -s main --no-print-director'])
