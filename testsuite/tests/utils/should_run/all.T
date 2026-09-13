test('T15953', [ignore_stdout, js_skip], makefile_test, [])

# Property tests for the 'Binary Literal' instance, which also exercises
# GHCs 'Binary Integer' instance.
test('Binary_Literal', [mini_quickcheck], multimod_compile_and_run,
     ['Binary_Literal', '-package ghc'])
