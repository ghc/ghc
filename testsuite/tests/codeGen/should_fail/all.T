# Tests for code generator and CMM parser

# Only the LLVM code generator consistently forces the alignment of
# memcpy operations
test('T8131', [cmm_src, only_ways(llvm_ways)], compile_fail, ['-no-hs-main'])

def check_bounds_test(name):
    """ A -fcheck-prim-bounds test that is expected to fail. """
    # The native backend exits with EXIT_FAILURE (1); the JS backend still
    # aborts with h$exitProcess(134) (the improved messages are native-only).
    test(name,
         [ignore_stderr, omit_ghci, exit_code(134 if js_arch() else 1)],
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
check_bounds_test('T26958')

# Unlike the check_bounds_test cases above, these tests pin down the exact
# -fcheck-prim-bounds error message. Drop the leading "<prog>: " prefix so the
# tests don't depend on the binary's name/path (incl. Windows drive letters).
def strip_prog_prefix(s):
    return re.sub(r'(?m)^.*?(\w+#: array access)', r'\1', s)

# T26964 checks the single-element message. T26964b checks the range variant.
# Skipped on the JS backend, which emits no message and aborts with code 134.
test('T26964',
     [omit_ghci, js_skip, exit_code(1), normalise_errmsg_fun(strip_prog_prefix)],
     compile_and_run, ['-fcheck-prim-bounds'])
test('T26964b',
     [omit_ghci, js_skip, exit_code(1), normalise_errmsg_fun(strip_prog_prefix)],
     compile_and_run, ['-fcheck-prim-bounds'])
# T26964Module checks that the reported module is the one containing the failing
# primop (the helper T26964ModuleA), not the Main module that calls it.
test('T26964Module',
     [omit_ghci, js_skip, exit_code(1), normalise_errmsg_fun(strip_prog_prefix),
      extra_files(['T26964ModuleA.hs'])],
     multimod_compile_and_run, ['T26964Module', '-fcheck-prim-bounds'])
