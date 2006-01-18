# exhausts Hugs's heap (CAF leak)
test('life_space_leak', compose(skip_if_fast,expect_fail_if_compiler_type('hugs')),
	multimod_compile_and_run, ['Main', ''])
clean_o_hi()

