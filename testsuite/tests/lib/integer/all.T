test('integerBits', normal, compile_and_run, [''])
test('integerConversions', normal, compile_and_run, [''])
# skip ghci as it doesn't support unboxed tuples
test('integerGmpInternals', [reqlib('integer-gmp'), omit_ways('ghci')], compile_and_run, [''])
test('plusMinusInteger', [omit_ways('ghci')], compile_and_run, [''])
test('integerConstantFolding', broken_without_gmp, run_command,
     ['$MAKE -s --no-print-directory integerConstantFolding'])
test('fromToInteger', [], run_command,
     ['$MAKE -s --no-print-directory fromToInteger'])
test('IntegerConversionRules', [], run_command,
     ['$MAKE -s --no-print-directory IntegerConversionRules'])
test('gcdInteger', normal, compile_and_run, [''])
