# "Unit tests"

# Regression tests
test('T21652', [ only_ways(['optasm']) ], compile_and_run, [''])
test('T21694a', [ only_ways(['optasm']), exit_code(1) ], compile_and_run, ['-fpedantic-bottoms'])
test('T24296', [ only_ways(['optasm']), exit_code(1) ], compile_and_run, ['-fpedantic-bottoms'])

