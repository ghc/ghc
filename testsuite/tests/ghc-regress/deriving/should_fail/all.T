
test('drvfail001', normal, compile_fail, [''])
test('drvfail002', normal, compile_fail, [''])
test('drvfail003', normal, compile_fail, [''])
test('drvfail004', normal, compile_fail, [''])
test('drvfail005', normal, compile_fail, [''])
test('drvfail006', reqlib('mtl'), compile_fail, [''])
test('drvfail007', normal, compile_fail, [''])
test('drvfail008', reqlib('mtl'), compile_fail, [''])
test('drvfail009', normal, compile_fail, [''])
test('drvfail010', normal, compile_fail, [''])
test('drvfail011', normal, compile_fail, [''])
test('drvfail012', normal, compile_fail, [''])
test('drvfail013', normal, compile_fail, [''])
test('drvfail014', normal, compile_fail, [''])
test('drvfail015', normal, compile_fail, [''])
test('drvfail016',
     extra_clean(['drvfail016.hi-boot', 'drvfail016.o-boot']),
     run_command,
     ['$MAKE --no-print-directory -s drvfail016'])
test('T2394', normal, compile_fail, [''])
test('T2604', normal, compile_fail, [''])
test('T2701', normal, compile_fail, [''])
test('T2851', normal, compile_fail, [''])
test('T2721', normal, compile_fail, [''])
test('T3101', normal, compile_fail, [''])
test('drvfail-functor1',
     if_compiler_lt('ghc', '6.11', expect_fail),
     compile_fail,
     [''])
test('drvfail-functor2',
     if_compiler_lt('ghc', '6.11', expect_fail),
     compile_fail, [''])
test('drvfail-foldable-traversable1',
     if_compiler_lt('ghc', '6.11', expect_fail),
     compile_fail,
     [''])
