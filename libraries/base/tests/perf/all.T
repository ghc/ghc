# .stats files aren't yet supported in the JS backend
setTestOpts(js_skip)

#--------------------------------------
# Check specialization of elem via rules
#--------------------------------------

test('T17752', [only_ways(['normal'])] , makefile_test, ['T17752'])

#--------------------------------------

# We don't expect the code in test to vary at all, but the variance is set to
# 1% in case the constant allocations increase by other means.
test('encodingAllocations', [only_ways(['normal']), collect_stats('bytes allocated', 1)], compile_and_run, ['-O2'])
