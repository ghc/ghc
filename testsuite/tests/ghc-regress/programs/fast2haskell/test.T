
test('fast2haskell', skip_if_fast, multimod_compile_and_run, \
	['Main', '-fglasgow-exts'])
clean_o_hi()

