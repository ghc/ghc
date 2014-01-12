# exhausts Hugs's heap (CAF leak)
test('life_space_leak',
     [when(fast(), skip),
      extra_clean(['Main.hi', 'Main.o']),
      when(compiler_type('hugs'), expect_fail)],
     multimod_compile_and_run,
     ['Main', ''])

