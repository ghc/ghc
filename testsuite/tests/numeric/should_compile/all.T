test('T7116', normal, makefile_test, ['T7116'])
# These test Core output that depends upon integer-gmp
test('T14170', normal, makefile_test, ['T14170'])
test('T14465', reqlib("integer-gmp"), makefile_test, ['T14465'])
test('T7895', normal, compile, [''])
test('T7881', normal, compile, [''])
# For T8542, the hpc way adds extra annotations that prevent
# the appropriate expression to be detected, after
# desugaring, so we don't get the warning we expect.
test('T8542', omit_ways(['hpc']), compile, [''])
test('T10929', normal, compile, [''])
test('T16402', [ grep_errmsg(r'and') ], compile, [''])
