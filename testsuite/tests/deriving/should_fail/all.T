
test('drvfail001', normal, compile_fail, [''])
test('drvfail002', normal, compile_fail, [''])
test('drvfail003', normal, compile_fail, [''])
test('drvfail004', normal, compile_fail, [''])
test('drvfail005', normal, compile_fail, [''])
test('drvfail006', reqlib('mtl'), compile_fail, [''])
test('drvfail007', normal, compile_fail, [''])
test('drvfail008', reqlib('mtl'), compile_fail, [''])
test('drvfail009', normal, compile_fail, [''])
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
test('T3621', normal, compile_fail, [''])
test('drvfail-functor1', normal, compile_fail, [''])
test('drvfail-functor2', normal, compile_fail, [''])
test('drvfail-foldable-traversable1', normal, compile_fail,
     [''])
test('T3833', normal, compile_fail, [''])
test('T3834', normal, compile_fail, [''])
test('T4528', normal, compile_fail, [''])
test('T5287', normal, compile_fail, [''])
test('T5478', normal, compile_fail, [''])
test('T5686', normal, compile_fail, [''])
test('T5922', normal, compile_fail, [''])
test('T1133A',
     extra_clean(['T1133A.o-boot', 'T1133A.hi-boot']),
     run_command,
     ['$MAKE --no-print-directory -s T1133A'])
test('T5863a', normal, compile_fail, [''])
test('T7959', normal, compile_fail, [''])

test('T1496', normal, compile_fail, [''])
test('T4846', normal, compile_fail, [''])
test('T7148', normal, compile_fail, [''])
test('T7148a', normal, compile_fail, [''])
test('T7800', normal, multimod_compile_fail, ['T7800',''])
