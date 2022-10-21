test('sum_mod', [normalise_slashes, extra_files(['Lib.hs', 'Main.hs']), js_skip],
     run_command, ['$MAKE -s main --no-print-director'])
