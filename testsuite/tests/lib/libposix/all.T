test('posix002', [ reqlib('unix'), omit_ways(prof_ways) ],
                 compile_and_run, [''])

# Skip on mingw32: assumes existence of 'pwd' and /tmp
test('posix003', if_os('mingw32', skip), compile_and_run, [''])

# Uses forkProcess; doesn't work with threaded2
test('posix004', [ reqlib('unix'), expect_fail_for(['threaded2']) ],
                 compile_and_run, [''])

test('posix005', [reqlib('unix'), expect_broken(5648)], compile_and_run, [''])

test('posix006', reqlib('unix'), compile_and_run, [''])
test('posix009', [ omit_ways(threaded_ways), reqlib('unix') ], compile_and_run, [''])
test('posix010', reqlib('unix'), compile_and_run, [''])

# Uses forkProcess; doesn't work with threaded2
test('posix014', [ reqlib('unix'), expect_fail_for(['threaded2']) ],
                 compile_and_run, [''])
