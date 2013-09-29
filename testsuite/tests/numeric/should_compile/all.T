test('T7116', normal, run_command, ['$MAKE -s --no-print-directory T7116'])
test('T7895', normal, compile, [''])
test('T7881', [expect_broken_for('8384','hpc')], compile, [''])
