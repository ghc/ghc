# pass $testdir so it knows where its home dir is

# Expect failure when running the optasm way, due to floating-point
# differences.
test('galois_raytrace', \
	compose(omit_ways(['normal']), expect_fail_for(['optasm'])), \
	multimod_compile_and_run, \
	['Main', '-O -package lang -package text'])
clean_o_hi()

