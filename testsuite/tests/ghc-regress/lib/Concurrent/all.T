test('SampleVar001', normal, compile_and_run, ['-package QuickCheck'])
test('Chan001', normal, compile_and_run, ['-package QuickCheck'])
test('MVar001', normal, compile_and_run, ['-package QuickCheck'])

# Skipped for now... needs getQuantityQSemN function in QSemN.
test('QSemN001', skip, compile_and_run, ['-package QuickCheck'])

# Skipped for now... needs getQuantityQSem function in QSem.
test('QSem001', skip, compile_and_run, ['-package QuickCheck'])
