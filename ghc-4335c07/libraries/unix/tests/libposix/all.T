test('posix002', [ reqlib('unix'), omit_ways(prof_ways) ],
                 compile_and_run, [''])

# Skip on mingw32: assumes existence of 'pwd' and /tmp
test('posix003', [when(opsys('mingw32'), skip), extra_clean(['po003.out'])],
                 compile_and_run, [''])

test('posix004', [ reqlib('unix') ], compile_and_run, [''])

test('posix005', [reqlib('unix') ], compile_and_run, [''])

test('posix006', reqlib('unix'), compile_and_run, [''])
test('posix009', [ omit_ways(threaded_ways), reqlib('unix') ], compile_and_run, [''])
test('posix010', reqlib('unix'), compile_and_run, [''])

test('posix014', [ reqlib('unix') ], compile_and_run, [''])
