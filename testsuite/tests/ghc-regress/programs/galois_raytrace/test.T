setTestOpts(omit_compiler_types(['hugs']))	# takes much too long

test('galois_raytrace', \
	compose(skip_if_fast, reqlib('parsec')), \
	multimod_compile_and_run, \
	['Main','-package parsec'])
clean_o_hi()
