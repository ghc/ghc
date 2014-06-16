# exhausts Hugs's heap
test('jules_xref',
     [when(fast(), skip),
      extra_clean(['Main.hi', 'Main.o']),
      omit_compiler_types(['hugs'])],
     multimod_compile_and_run,
     ['Main', ''])

