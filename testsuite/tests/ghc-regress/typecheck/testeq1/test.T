
test('typecheck.testeq1', skip_if_fast, multimod_compile_and_run, \
	['Main', '-v0 -fglasgow-exts'])
clean(['Main.hi', 'Main.o', 'TypeCast.hi', 'TypeCast.o', 'FakePrelude.hi', 'FakePrelude.o', 'TypeEq.hi', 'TypeEq.o'])
