# exhausts Hugs's heap
test('jules_xref', omit_compiler_types(['hugs']), multimod_compile_and_run, ['Main', ''])
clean_o_hi()

