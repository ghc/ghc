test('naturalConstantFolding', [], run_command,
     ['$MAKE -s --no-print-directory naturalConstantFolding'])
# See Note [Constant folding of @Natural@/@Integer@->@Int64@/@Word64@ conversions on 32-bit architectures]
# for why this test is needed, and why it should be allowed to fail in 32-bit
# architectures.
# The conversions to 64-bit ints/words are tested separately since only they
# may be allowed to fail in case the architecture is 32-bit, no other natural
# constant folding test is.
test('naturalToIntWord64ConstantFolding',
     [when(arch('i386'), expect_broken(15821)) ],
     run_command,
     ['$MAKE -s --no-print-directory naturalToIntWord64ConstantFolding'])