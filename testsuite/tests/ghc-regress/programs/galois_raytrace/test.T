
# pass $testdir so it knows where its home dir is
test('galois_raytrace', omit_ways(['normal']), multimod_compile_and_run, \
	['Main', '-O -package lang -package text'])

