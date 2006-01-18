# exhausts Hugs's heap
test('jules_xref', compose(skip_if_fast,omit_compiler_types(['hugs'])), multimod_compile_and_run, ['Main', ''])
clean_o_hi()

