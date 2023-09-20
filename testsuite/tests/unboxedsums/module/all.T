test('sum_mod', [normalise_slashes, extra_files(['Lib.hs', 'Main.hs'])],
     makefile_test, ['main'])
