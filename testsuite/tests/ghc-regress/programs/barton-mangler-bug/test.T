# Expect failure when running the optasm way, due to floating-point
# differences.
# Also exhausts Hugs's heap (CAF leak)
test('barton-mangler-bug',
	compose(expect_fail_for(['optasm']), omit_compiler_types(['hugs'])),
	multimod_compile_and_run, ['Main', ''])
clean_o_hi()
