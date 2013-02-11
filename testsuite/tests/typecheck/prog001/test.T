
test('typecheck.prog001',
     [when(fast(), skip),
      extra_clean(['A.hi', 'A.o', 'B.hi', 'B.o', 'C.hi', 'C.o'])],
     multimod_compile,
	 ['C', '-v0'])
