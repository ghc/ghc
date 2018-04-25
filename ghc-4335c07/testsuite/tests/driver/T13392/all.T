# Test for #13392, it makes sure 'ghc --show-options' does not print each flag more than once.

test('T13392', normal, run_command, ['$MAKE -s --no-print-directory T13392'])
