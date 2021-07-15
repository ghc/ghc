test('T20030_test2',
     [ extra_files([ 'L.hs', 'L.hs-boot', 'M.hs', 'M.hs-boot', 'O.hs', 'O.hs-boot' ])
     ],
     multimod_compile, ['O.hs', '-v1'])
