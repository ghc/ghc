test('pkg02',
     [reqlib('network'),
      extra_clean(['A.o', 'A.hi', 'Network.o', 'Network.hi'])],
     multimod_compile,
     ['A','-v0'])

