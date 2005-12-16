
test('fast2haskell', normal, multimod_compile_and_run, \
	['Main', '-fglasgow-exts'])
clean_o_hi()

