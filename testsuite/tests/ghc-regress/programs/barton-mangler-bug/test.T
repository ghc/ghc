# Expect failure when running the optasm way, due to floating-point
# differences.
# Also exhausts Hugs's heap (CAF leak)

if config.platform != 'x86_64-unknown-linux':
	ways = expect_fail_for(['optasm'])
else:
	ways = normal

test('barton-mangler-bug',
	compose(ways, omit_compiler_types(['hugs'])),
	multimod_compile_and_run, ['Main', ''])
clean_o_hi()
