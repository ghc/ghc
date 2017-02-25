test('overloadedrecfldsrun01', [], multimod_compile_and_run,
     ['overloadedrecfldsrun01', ''])
test('overloadedrecfldsrun02', [extra_files(['OverloadedRecFldsRun02_A.hs'])], multimod_compile_and_run,
     ['overloadedrecfldsrun02', ''])
test('overloadedrecfldsrun03', normal, compile_and_run, [''])
test('overloadedrecfldsrun04', omit_ways(prof_ways), compile_and_run, [''])
test('overloadedrecfldsrun05', normal, compile_and_run, [''])
test('overloadedrecfldsrun06', normal, compile_and_run, [''])
test('overloadedrecfldsrun07', normal, compile_and_run, [''])
test('overloadedrecflds_generics', normal, compile_and_run, [''])
test('overloadedlabelsrun01', normal, compile_and_run, [''])
test('overloadedlabelsrun02', normal, compile_and_run, [''])
test('overloadedlabelsrun03', normal, compile_and_run, [''])
test('overloadedlabelsrun04', [extra_files(['OverloadedLabelsRun04_A.hs']),
                               omit_ways(prof_ways)], multimod_compile_and_run,
     ['overloadedlabelsrun04', config.ghc_th_way_flags])
test('hasfieldrun01', normal, compile_and_run, [''])
test('hasfieldrun02', normal, compile_and_run, [''])
test('T12243', normal, compile_and_run, [''])
