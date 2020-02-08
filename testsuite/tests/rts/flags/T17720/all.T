test('T17720', 
    [extra_run_opts('+RTS -ibogus'), 
        exit_code(1), check_errmsg('bad value for -i'), only_ways(['normal'])],
    compile_and_run,
    ['-rtsopts'])