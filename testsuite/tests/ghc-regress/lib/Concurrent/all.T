setTestOpts(only_compiler_types(['ghc']))

test('SampleVar001', reqlib('QuickCheck'), compile_and_run, ['-package QuickCheck'])
test('Chan001',      reqlib('QuickCheck'), compile_and_run, ['-package QuickCheck'])
test('MVar001',      reqlib('QuickCheck'), compile_and_run, ['-package QuickCheck'])
test('QSemN001',     reqlib('QuickCheck'), compile_and_run, ['-package QuickCheck'])
test('QSem001',      reqlib('QuickCheck'), compile_and_run, ['-package QuickCheck'])
test('ThreadDelay001', normal, compile_and_run, [''])
