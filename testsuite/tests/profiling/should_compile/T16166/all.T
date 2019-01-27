# We need the register allocator to use more than a page worth of stack space
# when spilling in a single function, easiest way to do that is
# using a profiling build
test('T16166', [only_ways(['normal']), req_profiling,
                extra_files(['Main.hs', 'NetworkRequestHeader.hs'])],
     multimod_compile,
     ['Main NetworkRequestHeader', '-O -prof -fprof-auto -v0'])
