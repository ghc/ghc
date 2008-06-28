# exhausts Hugs's heap (CAF leak)
test('life_space_leak',
     [skip_if_fast,
      extra_clean(['Main.hi', 'Main.o']),
      if_compiler_type('hugs', expect_fail)],
     multimod_compile_and_run,
     ['Main', ''])

