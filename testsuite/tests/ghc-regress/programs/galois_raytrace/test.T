setTestOpts(omit_compiler_types(['hugs']))	# takes much too long

test('galois_raytrace', \
	normal, \
	multimod_compile_and_run, \
	['Main', '-O -package lang -package text'])
clean_o_hi()

