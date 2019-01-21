# Test that we don't load all family instance interface files
# when importing a module that defines a family instance

test('T13092b', ignore_stdout, makefile_test, ['T13092b'])
