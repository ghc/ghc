test('prog003',
     extra_clean(['D.hs', 'D.hi', 'C.hi', 'C.o', 'B.hi', 'B.o',
                  'A', 'A.hi', 'A.o', 'a.out']),
     ghci_script,
     ['prog003.script'])

