
test('fast2haskell', normal, multimod_compile_and_run, \
	['Main', '-fglasgow-exts -package lang'])
clean_o_hi()

