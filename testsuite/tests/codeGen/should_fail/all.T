# Tests for code generator and CMM parser

# Only the LLVM code generator consistently forces the alignment of
# memcpy operations
test('T8131', [cmm_src, only_ways(llvm_ways)], compile_fail, [''])
test('T13233', normal, compile_fail, [''])
test('T13233_elab', normal, compile_fail, ['-fprint-typechecker-elaboration'])
