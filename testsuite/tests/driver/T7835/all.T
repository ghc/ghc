
test('T7835', [extra_files(['Test.hs', 'TestPrim.hs', 'test-prims.cmm'])], run_command, ['$MAKE -s --no-print-directory T7835'])
