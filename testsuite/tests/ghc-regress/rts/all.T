test('testblockalloc', compose(c_src, only_ways(['normal'])), 
                       compile_and_run, [''])

test('testblockalloc', compose(c_src, 
                       compose(only_ways(['threaded1']),
                               extra_run_opts('+RTS -I0'))), 
                       compile_and_run, [''])


# See bug #101, test requires +RTS -c (or equivalently +RTS -M<something>)
# only GHCi triggers the bug, but we run the test all ways for completeness.
test('bug1010', normal, compile_and_run, ['+RTS -c -RTS'])
