
test('T7835',
     extra_clean(['Test', 'Test.exe', 'Test.hi', 'Test.o',
                  'TestPrim.hi', 'TestPrim.o', 'test-prims.o']),
     run_command,
     ['$MAKE -s --no-print-directory T7835'])

