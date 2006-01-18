
test('andy_cherry', compose(skip_if_fast,extra_run_opts('.')), \
	multimod_compile_and_run, \
	['Main', '-cpp'])
clean_o_hi()

