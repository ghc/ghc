test('T20030_test3',
     [ extra_files([ 'L.hs', 'L.hs-boot', 'M.hs', 'M.hs-boot', 'N.hs', 'N.hs-boot', 'O.hs', 'O.hs-boot' ])
     ],
     multimod_compile, ['O.hs N.hs', '-v1'])
