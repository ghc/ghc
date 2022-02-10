test('sum_mod', [normalise_slashes, extra_files(['Lib.hs', 'Main.hs']), js_broken(22261)],
     run_command, ['$MAKE -s main --no-print-director'])
