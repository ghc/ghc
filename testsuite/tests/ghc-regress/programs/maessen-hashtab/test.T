
test('maessen_hashtab', compose(skip_if_fast,extra_run_opts('99999')), 
	multimod_compile_and_run, ['HashTest', '-cpp'])
clean_o_hi()

