test('T20030_test4',
     [ extra_files([ 'L1_1.hs', 'L1.hs', 'L1.hs-boot', 'L2_1.hs', 'L2.hs',
     'L2.hs-boot', 'M.hs', 'UOL1_2.hs', 'UOL1.hs', 'UOL2.hs' ])
     ],
     multimod_compile, ['UOL1_2.hs UOL1.hs UOL2.hs', '-v1'])

