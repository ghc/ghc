
test('callback', omit_ways(['optasm','ghci']), multimod_compile_and_run, \
	['Main', '-fglasgow-exts -package lang -fvia-C'])
