# exhausts Hugs's heap
test('jules_xref',
     [skip_if_fast,
      extra_clean(['Main.hi', 'Main.o']),
      omit_compiler_types(['hugs'])],
     multimod_compile_and_run,
     ['Main', ''])

