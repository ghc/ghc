setTestOpts(omit_compiler_types(['hugs']))	# takes much too long

# Floating point differences on x86 using the NCG
if config.platform.startswith('i386-') and \
       config.platform != 'i386-unknown-openbsd':
    setTestOpts(expect_fail_for(['hpc','optasm','profasm','threaded2']))

test('galois_raytrace', \
	compose(skip_if_fast, reqlib('parsec')), \
	multimod_compile_and_run, \
	['Main','-package parsec'])
clean_o_hi()
