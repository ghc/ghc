test('testblockalloc', compose(c_src,
                               only_ways(['normal','threaded1'])), 
                       compile_and_run, [''])
