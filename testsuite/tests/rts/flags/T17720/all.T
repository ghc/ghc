test('T17720a', 
    [extra_run_opts('+RTS -ibogus'), 
        exit_code(1), check_errmsg('bad value for -i'), extra_files(['T17720.hs']), 
        only_ways(['normal'])],
    multimod_compile_and_run,
    ['T17720', '-rtsopts'])

test('T17720b', 
    [extra_run_opts('+RTS -Cv'), 
        exit_code(1), check_errmsg('bad value for -C'), extra_files(['T17720.hs']), 
        only_ways(['normal'])],
    multimod_compile_and_run,
    ['T17720', '-rtsopts'])

test('T17720c', 
    [extra_run_opts('+RTS -V3b'), 
        exit_code(1), check_errmsg('bad value for -V'), extra_files(['T17720.hs']), 
        only_ways(['normal'])],
    multimod_compile_and_run,
    ['T17720', '-rtsopts'])