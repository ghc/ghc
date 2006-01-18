test('joao-circular', compose(skip_if_fast,extra_run_opts('inp 40')), multimod_compile_and_run, ['Main', '-O'])
clean_o_hi()
