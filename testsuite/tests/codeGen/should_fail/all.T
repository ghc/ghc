# Tests for code generator and CMM parser

# Only the LLVM code generator consistently forces the alignment of
# memcpy operations
test('T8131', [cmm_src, only_ways(llvm_ways)], compile_fail, ['-no-hs-main'])

def check_bounds_test(name):
    """ A -fcheck-prim-bounds test that is expected to fail. """
    test(name,
         [ignore_stderr, exit_code(127 if opsys('mingw32') else 134)],
         compile_and_run, ['-fcheck-prim-bounds'])

check_bounds_test('CheckBoundsWriteArray') # Check past end
check_bounds_test('CheckBoundsIndexArray') # Check past end
check_bounds_test('CheckBoundsReadSmallArray') # Check before start
check_bounds_test('CheckBoundsReadInt8Array')
check_bounds_test('CheckBoundsReadInt64Array')  # read past end
check_bounds_test('CheckBoundsReadWord64Array') # read before start
check_bounds_test('CheckBoundsReadWord8ArrayAsInt32')  # Check last byte
check_bounds_test('CheckBoundsReadWord8ArrayAsWord32') # Check first byte
check_bounds_test('CheckBoundsCopyByteArray')
check_bounds_test('CheckBoundsCompareByteArray')  # Check last byte, 2nd array
check_bounds_test('CheckBoundsCompareByteArray2') # Check first byte, 1st array
check_bounds_test('CheckBoundsCompareByteArray3') # Check negative length
check_bounds_test('CheckOverlapCopyByteArray')
check_bounds_test('CheckOverlapCopyAddrToByteArray')
