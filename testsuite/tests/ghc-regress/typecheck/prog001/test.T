
test('typecheck.prog001',
     [skip_if_fast,
      extra_clean(['A.hi', 'A.o', 'B.hi', 'B.o', 'C.hi', 'C.o'])],
     multimod_compile,
	 ['C', '-v0'])
