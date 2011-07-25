test('integerBits', normal, compile_and_run, [''])
test('integerConversions', normal, compile_and_run, [''])
test('integerConstantFolding',
     extra_clean(['integerConstantFolding.simpl']),
     run_command,
     ['$MAKE -s --no-print-directory integerConstantFolding'])

