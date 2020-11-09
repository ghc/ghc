# Verify that we optimize away conditional branches which always jump
# to the same target.
test('T15188', normal, makefile_test, [])
test('T18141', normal, compile, [''])
