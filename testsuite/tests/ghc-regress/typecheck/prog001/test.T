
test('typecheck.prog001', normal, multimod_compile, \
	['C', '-v0 -fglasgow-exts'])
clean(['A.hi', 'A.o', 'B.hi', 'B.o', 'C.hi', 'C.o'])
