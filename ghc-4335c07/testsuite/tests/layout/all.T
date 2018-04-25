
test('layout001', [], run_command, ['$MAKE -s --no-print-directory layout001'])

test('layout002', [], run_command, ['$MAKE -s --no-print-directory layout002'])

test('layout003', [], run_command, ['$MAKE -s --no-print-directory layout003'])

test('layout004', [], run_command, ['$MAKE -s --no-print-directory layout004'])

test('layout005', [], run_command, ['$MAKE -s --no-print-directory layout005'])

test('layout006', [], run_command, ['$MAKE -s --no-print-directory layout006'])

test('layout007', [req_interp], run_command,
     ['$MAKE -s --no-print-directory layout007'])

test('layout008', [], run_command, ['$MAKE -s --no-print-directory layout008'])

test('layout009', [], run_command, ['$MAKE -s --no-print-directory layout009'])
