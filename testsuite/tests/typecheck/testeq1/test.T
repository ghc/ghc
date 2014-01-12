
test('typecheck.testeq1',
     [when(fast(), skip),
      extra_clean(['Main.hi', 'Main.o',
                   'TypeCast.hi', 'TypeCast.o',
                   'FakePrelude.hi', 'FakePrelude.o',
                   'TypeEq.hi', 'TypeEq.o'])],
     multimod_compile_and_run,
     ['Main', '-v0'])
