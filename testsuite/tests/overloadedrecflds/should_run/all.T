test('overloadedrecfldsrun01',
     extra_clean(['OverloadedRecFldsRun01_A.hi', 'OverloadedRecFldsRun01_A.o']),
     multimod_compile_and_run, ['overloadedrecfldsrun01', ''])
test('overloadedrecfldsrun02',
     extra_clean(['OverloadedRecFldsRun02_A.hi', 'OverloadedRecFldsRun02_A.o']),
     multimod_compile_and_run, ['overloadedrecfldsrun02', ''])
test('overloadedrecfldsrun03', normal, compile_and_run, [''])
test('overloadedrecfldsrun04', normal, compile_and_run, [''])
test('overloadedrecfldsrun05', normal, compile_and_run, [''])
test('overloadedrecfldsrun06', normal, compile_and_run, [''])
test('overloadedrecflds_generics', normal, compile_and_run, [''])
test('overloadedlabelsrun01', normal, compile_and_run, [''])
test('overloadedlabelsrun02', normal, compile_and_run, [''])
test('overloadedlabelsrun03', normal, compile_and_run, [''])
test('overloadedlabelsrun04',
     extra_clean(['OverloadedLabelsRun04_A.hi', 'OverloadedLabelsRun04_A.o']),
     multimod_compile_and_run, ['overloadedlabelsrun04', ''])
