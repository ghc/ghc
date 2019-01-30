
test('T3807', [req_shared_libs, when(opsys('mingw32'), skip)], run_command,
     ['$MAKE --no-print-directory -s T3807'])

test('T4464', [req_shared_libs, unless(opsys('mingw32'), skip)], run_command,
     ['$MAKE --no-print-directory -s T4464'])

test('T5373', [req_shared_libs], run_command,
     ['$MAKE --no-print-directory -s T5373'])

# It's not clear exactly what platforms we can expect this to succeed on.
test('T13702', unless(opsys('linux'), skip), run_command,
     ['$MAKE --no-print-directory -s T13702'])
