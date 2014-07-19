test('cg001', only_compiler_types(['ghc']), compile, [''])
test('cg002', normal, compile, [''])
test('cg003', only_ways(['optasm']), compile, [''])
test('cg004', normal, compile, [''])
test('cg005', only_ways(['optasm']), compile, [''])
test('cg006', normal, compile, [''])
test('cg007', normal, compile, [''])
test('cg008', normal, compile, [''])

test('T1916', normal, compile, [''])
test('T2388', normal, compile, [''])
test('T3132', normal, compile, ['-dcmm-lint'])
test('T3286', extra_clean(['T3286b.o','T3286b.hi']), 
              multimod_compile, ['T3286', '-v0'])
test('T3579', normal, compile, [''])
test('T2578', normal, run_command, ['$MAKE -s --no-print-directory T2578'])
# skip llvm on i386 as we don't support fPIC
test('jmp_tbl', when(arch('i386'), omit_ways(['llvm', 'optllvm'])), compile, ['-fPIC -O'])
test('massive_array',
     [ when(arch('i386'), omit_ways(['llvm', 'optllvm'])) ],
     compile, ['-fPIC'])
test('T7237', normal, compile, [''])
test('T7574', [cmm_src, omit_ways(['llvm', 'optllvm'])], compile, [''])
test('T8205', normal, compile, ['-O0'])
test('T9155', normal, compile, ['-O2'])
