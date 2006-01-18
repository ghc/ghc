
test('typecheck.prog001', skip_if_fast, multimod_compile, \
	['C', '-v0 -fglasgow-exts'])
clean(['A.hi', 'A.o', 'B.hi', 'B.o', 'C.hi', 'C.o'])
