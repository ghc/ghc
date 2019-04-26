test('integerBits', normal, compile_and_run, [''])
test('integerConversions', normal, compile_and_run, [''])
# skip ghci as it doesn't support unboxed tuples
test('integerGmpInternals', [reqlib('integer-gmp'), omit_ways('ghci')], compile_and_run, [''])
test('plusMinusInteger', [omit_ways('ghci')], compile_and_run, [''])
test('integerConstantFolding', normal, makefile_test, ['integerConstantFolding'])
# See Note [Constant folding of @Natural@/@Integer@->@Int64@/@Word64@ conversions on 32-bit architectures]
# for why this test is needed, and why it should be allowed to fail in 32-bit
# architectures.
# The conversions to 64-bit ints/words are tested separately since only they
# may be allowed to fail in case the architecture is 32-bit, no other integer
# constant folding test is.
test('integerToIntWord64ConstantFolding',
     [when(arch('i386'), expect_broken(15821)) ],
     run_command,
     ['$MAKE -s --no-print-directory integerToIntWord64ConstantFolding'])
test('fromToInteger', [], makefile_test, ['fromToInteger'])
test('IntegerConversionRules', [], makefile_test, ['IntegerConversionRules'])
test('gcdInteger', normal, compile_and_run, [''])
