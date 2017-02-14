# Test that we don't load all family instance interface files
# when importing a module that defines a family instance

test('T13092b', ignore_stdout,
     run_command, ['$MAKE -s --no-print-directory T13092b'])
