# exhausts Hugs's heap (CAF leak)
test('andre_monad',
     [skip_if_fast,
      extra_clean(['Main.hi', 'Main.o']),
      omit_compiler_types(['hugs'])],
     multimod_compile_and_run,
     ['Main', ''])

