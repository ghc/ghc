test('cg001', normal, compile, [''])
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
test('T3286', [], multimod_compile, ['T3286', '-v0'])
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
test('T9303', normal, compile, ['-O2'])
test('T9329', [cmm_src], compile, [''])

test('debug',
     [when((arch('powerpc64') or arch('powerpc64le')), expect_broken(11261))],
     run_command, ['$MAKE -s --no-print-directory debug'])
test('T9964', normal, compile, ['-O'])
test('T10518', [cmm_src], compile, [''])
test('T10667', [ when((arch('powerpc64') or arch('powerpc64le')),
                      expect_broken(11261))],
     compile, ['-g'])
test('T12115', normal, compile, [''])
test('T12355', normal, compile, [''])
test('T14999',
     [when((arch('powerpc64') or arch('powerpc64le')), expect_broken(11261)),
      unless(opsys('linux') and arch('x86_64') and have_gdb() and
        have_readelf(), skip)],
     run_command, ['$MAKE -s --no-print-directory T14999'])

# Verify that we optimize away redundant jumps for unordered comparisons.
test('T15196',
  [ unless(arch('x86_64'),skip),
    only_ways('normal'),
  ], run_command, ['$MAKE -s --no-print-directory T15196'])
