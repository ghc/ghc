test('bytestring001',
     [skip, # This is designed for an earlier version of bytestring
      reqlib('QuickCheck')],
     compile_and_run,
     ['-package bytestring -package QuickCheck'])
test('bytestring002', normal, compile_and_run, ['-package bytestring'])
test('bytestring003', normal, compile_and_run, ['-package bytestring'])
test('bytestring004',
     [skip, # This is designed for an earlier version of bytestring
      reqlib('QuickCheck')],
     compile_and_run,
     ['-package bytestring -package QuickCheck'])
test('bytestring005',
     [skip, # This is designed for an earlier version of bytestring
      reqlib('QuickCheck')],
     compile_and_run,
     ['-package bytestring -package QuickCheck'])
test('bytestring006', normal, compile_and_run, ['-package bytestring'])
