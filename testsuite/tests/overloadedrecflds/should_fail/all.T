test('overloadedrecfldsfail01', normal, compile_fail, [''])
test('overloadedrecfldsfail02', normal, compile_fail, [''])
test('overloadedrecfldsfail03', normal, compile_fail, [''])
test('overloadedrecfldsfail04', [extra_files(['OverloadedRecFldsFail04_A.hs'])], multimod_compile_fail,
     ['overloadedrecfldsfail04', ''])
test('overloadedrecfldsfail05', normal, compile_fail, [''])
test('overloadedrecfldsfail06', [extra_files(['OverloadedRecFldsFail06_A.hs'])], multimod_compile_fail,
     ['overloadedrecfldsfail06', ''])
test('overloadedrecfldsfail07', normal, compile_fail, [''])
test('overloadedrecfldsfail08', normal, compile_fail, [''])
test('overloadedrecfldsfail09', normal, compile_fail, [''])
test('overloadedrecfldsfail10', [extra_files(['OverloadedRecFldsFail10_A.hs', 'OverloadedRecFldsFail10_B.hs', 'OverloadedRecFldsFail10_C.hs'])], multimod_compile_fail,
     ['overloadedrecfldsfail10', ''])
test('overloadedrecfldsfail11', [extra_files(['OverloadedRecFldsFail11_A.hs'])], multimod_compile_fail,
     ['overloadedrecfldsfail11', ''])
test('overloadedrecfldsfail12', [extra_files(['OverloadedRecFldsFail12_A.hs'])], multimod_compile_fail,
     ['overloadedrecfldsfail12', ''])
test('overloadedrecfldsfail13', normal, compile_fail, [''])
test('overloadedrecfldsfail14', normal, compile_fail, [''])
test('overloadedlabelsfail01', normal, compile_fail, [''])
test('overloadedlabelsfail02', normal, compile_fail, [''])
test('overloadedlabelsfail03', normal, compile_fail, [''])
test('T11103', normal, compile_fail, [''])
test('T11167_ambiguous_fixity', [], multimod_compile_fail,
     ['T11167_ambiguous_fixity', ''])
test('T13132_duplicaterecflds', normal, compile_fail, [''])
test('NoParent', normal, compile_fail, [''])
test('hasfieldfail01', extra_files(['HasFieldFail01_A.hs']),
     multimod_compile_fail, ['hasfieldfail01', ''])
test('hasfieldfail02', normal, compile_fail, [''])
test('hasfieldfail03', normal, compile_fail, [''])
test('T14953', [extra_files(['T14953_A.hs', 'T14953_B.hs'])],
     multimod_compile_fail, ['T14953', ''])
test('DuplicateExports', normal, compile_fail, [''])
test('T17965', normal, compile_fail, [''])
test('DRFHoleFits', extra_files(['DRFHoleFits_A.hs']), multimod_compile_fail, ['DRFHoleFits', ''])
test('DRFPartialFields', normal, compile_fail, [''])
test('T16745', extra_files(['T16745C.hs', 'T16745B.hs']), multimod_compile_fail, ['T16745A', ''])
test('DRF9156', normal, compile_fail, [''])
