setTestOpts(omit_compiler_types(['hugs']))	# takes much too long

# Omit the asm ways on i386, because they generate different results due to
# implicit -fexcess-precision.
if config.platform.startswith('i386-'):
   setTestOpts(omit_ways(['optasm','profasm']))

test('galois_raytrace', \
	skip_if_fast, \
	multimod_compile_and_run, \
	['Main','-package parsec'])
clean_o_hi()
