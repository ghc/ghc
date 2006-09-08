test('prog003', normal, ghci_script, ['prog003.script'])
clean(['D.hi', 'C.hi', 'C.o', 'B.hi', 'B.o', 'A', 'A.hi', 'A.o', 'a.out'])

