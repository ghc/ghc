# Test that we don't load family instance instance files from
# external packages when defining an instance for a type family
# defined in the home package, but do read instance files from
# the home package. (Instance in T13092c_2 overlaps with the one
# defined in T13092c_4, and would not be read otherwise.)

test('T13092c', ignore_stdout,
     makefile_test, ['T13092c'])
