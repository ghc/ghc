test('prog001',
     extra_clean(['C.hs', 'D.hs', 'D.hi', 'D.o']),
     ghci_script, ['prog001.script'])

