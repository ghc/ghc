
test('callback', omit_ways(['optasm','ghci','extcore','optextcore']), multimod_compile_and_run, \
	['Main', '-fglasgow-exts -package lang -fvia-C'])
