setTestOpts(only_compiler_types(['ghc']))

test('SampleVar001', normal, compile_and_run, ['-package QuickCheck'])
test('Chan001', normal, compile_and_run, ['-package QuickCheck'])
test('MVar001', normal, compile_and_run, ['-package QuickCheck'])
test('QSemN001', normal, compile_and_run, ['-package QuickCheck'])
test('QSem001', normal, compile_and_run, ['-package QuickCheck'])
test('ThreadDelay001', normal, compile_and_run, [''])
