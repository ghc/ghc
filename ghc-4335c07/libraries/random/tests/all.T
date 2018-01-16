setTestOpts(reqlib('random'))

test('rangeTest',
     normal,
     compile_and_run,
     [''])

test('random1283',
     reqlib('containers'),
     compile_and_run,
     [' -package containers'])
