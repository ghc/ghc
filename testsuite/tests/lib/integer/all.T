test('integerBits', normal, compile_and_run, [''])
test('integerConversions', normal, compile_and_run, [''])
test('integerGmpInternals', reqlib('integer-gmp'), compile_and_run, [''])
test('integerConstantFolding',
     extra_clean(['integerConstantFolding.simpl']),
     run_command,
     ['$MAKE -s --no-print-directory integerConstantFolding'])
test('fromToInteger',
     extra_clean(['fromToInteger.simpl']),
     run_command,
     ['$MAKE -s --no-print-directory fromToInteger'])
test('IntegerConversionRules',
     extra_clean(['IntegerConversionRules.simpl']),
     run_command,
     ['$MAKE -s --no-print-directory IntegerConversionRules'])
test('gcdInteger', normal, compile_and_run, [''])

