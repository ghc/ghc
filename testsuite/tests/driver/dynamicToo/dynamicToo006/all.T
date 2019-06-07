test('dynamicToo006',
     [normalise_slashes, extra_files(['Main.hs']), unless(have_dynamic(), skip)],
     run_command, ['$MAKE -s main --no-print-director'])
