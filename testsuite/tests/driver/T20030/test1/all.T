test('T20030_test1',
     [ extra_files([ 'A.hs-boot' , 'A.hs' , 'B.hs' , 'C.hs-boot' , 'C.hs'
                   , 'D.hs' , 'E.hs-boot' , 'E.hs' , 'F.hs' , 'G.hs' , 'H.hs'
                   , 'I.hs', 'J.hs-boot', 'J.hs', 'K.hs' ])
     ],
     multimod_compile, ['I.hs K.hs', '-v1'])

test('T20030_test1j',
     [ extra_files([ 'A.hs-boot' , 'A.hs' , 'B.hs' , 'C.hs-boot' , 'C.hs'
                   , 'D.hs' , 'E.hs-boot' , 'E.hs' , 'F.hs' , 'G.hs' , 'H.hs'
                   , 'I.hs', 'J.hs-boot', 'J.hs', 'K.hs' ])
     , req_smp
     ],
     multimod_compile, ['I.hs K.hs', '-v1 -j'])
