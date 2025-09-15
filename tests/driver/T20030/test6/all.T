test('T20030_test6',
     [ extra_files([ 'L1_1.hs', 'L1_2.hs', 'L1.hs', 'L1.hs-boot', 'L2_1.hs', 'L2_2.hs', 'L2.hs',
     'L2.hs-boot', 'UOL1_2.hs', 'UOL1.hs', 'UOL1_1.hs', 'UOL2.hs' ])
     ],
     multimod_compile, ['UOL1_1.hs UOL1_2.hs UOL1.hs UOL2.hs', '-v1'])

