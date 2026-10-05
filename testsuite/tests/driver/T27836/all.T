test('T27836',
    [ extra_files(['A.hs', 'B.hs', 'C.hs', 'unitA', 'unitB', 'unitC']), normalise_slashes
    ],
    makefile_test, ['T27836'])
