# Tests for code generator and CMM parser

# Only the LLVM code generator consistently forces the alignment of
# memcpy operations
test('T8131', [cmm_src, only_ways(llvm_ways)], compile_fail, ['-no-hs-main'])

def check_bounds_test(name):
    """ A -fcheck-prim-bounds test that is expected to fail. """
    test(name,
         [ignore_stderr, exit_code(3 if opsys('mingw32') else 134)],
         compile_and_run, ['-fcheck-prim-bounds'])

check_bounds_test('CheckBoundsWriteArray')
check_bounds_test('CheckBoundsIndexArray')
check_bounds_test('CheckBoundsReadInt8Array')
check_bounds_test('CheckBoundsReadWord8ArrayAsInt32')
check_bounds_test('CheckBoundsCopyByteArray')
check_bounds_test('CheckBoundsCompareByteArray')

