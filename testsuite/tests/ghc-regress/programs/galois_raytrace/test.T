setTestOpts(omit_compiler_types(['hugs']))	# takes much too long

test('galois_raytrace', \
	skip_if_fast, \
	multimod_compile_and_run, \
	['Main','-package parsec'])
clean_o_hi()

