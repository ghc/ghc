# Expect failure when running the optasm way, due to floating-point
# differences. No fp failures on openbsd, for some reason.

if config.platform == 'i386-unknown-openbsd' or config.platform == 'x86_64-unknown-linux':
	ways = normal
else:
	ways = expect_fail_for(['optasm','profasm'])

test('cholewo-eval', compose(skip_if_fast,ways), multimod_compile_and_run, ['Main', ''])
clean_o_hi()
