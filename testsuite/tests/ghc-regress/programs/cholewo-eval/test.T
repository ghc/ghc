# Expect failure when running the optasm way, due to floating-point
# differences.
test('cholewo-eval', expect_fail_for(['optasm']), multimod_compile_and_run, ['Main', ''])
clean_o_hi()
