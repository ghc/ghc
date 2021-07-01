test('integerNegate', normal, compile, ['-ddump-simpl -dno-typeable-binds -dsuppress-all -dsuppress-uniques'])
test('integerBits', normal, compile_and_run, [''])
test('integerConversions', normal, compile_and_run, [''])
test('plusMinusInteger', [omit_ways(['ghci'])], compile_and_run, [''])
test('integerConstantFolding', normal, makefile_test, ['integerConstantFolding'])
test('naturalConstantFolding', normal, makefile_test, ['naturalConstantFolding'])

# we ignore_stderr because there are 2 overlapping rules that are reported in
# debug mode:
#   Rules.findBest: rule overlap (Rule 1 wins)
#     Rule 1: "Integer -> Int# (wrap)"
#     Rule 2: "Int# -> Integer -> Int#"
test('fromToInteger', [ignore_stderr], makefile_test, ['fromToInteger'])

test('IntegerConversionRules', [], makefile_test, ['IntegerConversionRules'])
test('gcdInteger', normal, compile_and_run, [''])
test('gcdeInteger', normal, compile_and_run, [''])
test('integerPowMod', [], compile_and_run, [''])
test('integerGcdExt', [], compile_and_run, [''])
test('integerRecipMod', [], compile_and_run, [''])
test('bignumMatch', [], compile, [''])
test('T18813', [], compile_and_run, [''])
test('T19170', [], compile_and_run, [''])

# skip ghci as it doesn't support unboxed tuples
test('integerImportExport', [omit_ways(['ghci'])], compile_and_run, [''])

# Disable GMP only tests
#test('integerGmpInternals', [], compile_and_run, [''])

test('T19345', [], compile_and_run, [''])
test('T20066', [exit_code(1)], compile_and_run, [''])
