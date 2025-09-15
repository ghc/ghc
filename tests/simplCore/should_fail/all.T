test('T7411', [expect_broken_for(7411, ['optasm', 'optllvm',
                                        'threaded2', 'dyn']),
               exit_code(1)], compile_and_run, [''])

test('T25117a', normal, compile_fail, [''])
test('T25117b', normal, compile_fail, [''])
# This one produces a warning
test('T25672', normal, compile, ['-O'])
