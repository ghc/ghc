test('determ021',
     [normalise_fun(normalise_errmsg), extra_files(['A.hs'])],
     run_command,
     ['$MAKE -s --no-print-directory determ021'])
