# Test for #1227
# We need to run with +RTS -hc -hbvoid -RTS, so we can't use the normal
# profiling ways which add their own RTS opts.  Hence we build the normal ways,
# adding our own profiling opts.
test('bio001',
     composes([only_ways(['normal', 'optc', 'optasm']),
               extra_run_opts('+RTS -hc -hbvoid -i0.01'),
               req_profiling]),
     compile_and_run, ['-prof -auto-all'])
