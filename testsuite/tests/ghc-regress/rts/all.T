test('testblockalloc', compose(c_src, only_ways(['normal'])), 
                       compile_and_run, [''])

test('testblockalloc', compose(c_src, 
                       compose(only_ways(['threaded1']),
                               extra_run_opts('+RTS -I0'))), 
                       compile_and_run, [''])
