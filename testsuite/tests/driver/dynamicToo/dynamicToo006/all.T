test('dynamicToo006',
     [normalise_slashes, extra_files(['Main.hs']), unless(have_dynamic(), skip)],
     makefile_test, ['main'])
