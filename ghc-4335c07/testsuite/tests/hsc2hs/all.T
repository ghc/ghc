
test('hsc2hs001', [], run_command, ['$MAKE -s --no-print-directory hsc2hs001'])

test('hsc2hs002', [], run_command, ['$MAKE -s --no-print-directory hsc2hs002'])

test('hsc2hs003', [], run_command, ['$MAKE -s --no-print-directory hsc2hs003'])

test('hsc2hs004', [], run_command, ['$MAKE -s --no-print-directory hsc2hs004'])


test('T3837', [], run_command, ['$MAKE -s --no-print-directory T3837'])

test('T4340', [], run_command, ['$MAKE -s --no-print-directory T4340'])

test('T10272', [], run_command, ['$MAKE -s --no-print-directory T10272'])

test('T12504', [extra_files(['T12504']), ignore_stdout], run_command,
     ['$MAKE -s --no-print-directory T12504'])
