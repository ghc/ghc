
test('jeff-bug', \
	compose(expect_fail, extra_run_opts('-count /dev/null'), \
	multimod_compile_and_run, \
	['Main', '-package lang -fglasgow-exts -cpp'])
