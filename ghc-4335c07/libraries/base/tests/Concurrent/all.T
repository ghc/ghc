test('Chan001',      reqlib('QuickCheck'), compile_and_run, ['-package QuickCheck'])

test('Chan002',      extra_run_opts('100'), compile_and_run, [''])
test('Chan003',      extra_run_opts('200'), compile_and_run, [''])

test('MVar001',      reqlib('QuickCheck'), compile_and_run, ['-package QuickCheck'])
test('ThreadDelay001', normal, compile_and_run, [''])
