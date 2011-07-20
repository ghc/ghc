
test('typecheck.testeq1',
     [skip_if_fast,
      extra_clean(['Main.hi', 'Main.o',
                   'TypeCast.hi', 'TypeCast.o',
                   'FakePrelude.hi', 'FakePrelude.o',
                   'TypeEq.hi', 'TypeEq.o'])],
     multimod_compile_and_run,
     ['Main', '-v0'])
