# exhausts Hugs's heap (CAF leak)
test('andre_monad', omit_compiler_types(['hugs']), multimod_compile_and_run, ['Main', ''])
clean_o_hi()

