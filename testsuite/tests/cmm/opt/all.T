# Verify that we optimize away conditional branches which always jump
# to the same target.
test('T15188', normal, run_command, ['$MAKE -s --no-print-directory T15188'])
