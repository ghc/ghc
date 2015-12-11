test('overloadedrecfldsfail01', normal, compile_fail, [''])
test('overloadedrecfldsfail02', normal, compile_fail, [''])
test('overloadedrecfldsfail03', normal, compile_fail, [''])
test('overloadedrecfldsfail04',
     extra_clean(['OverloadedRecFldsFail04_A.hi', 'OverloadedRecFldsFail04_A.o']),
     multimod_compile_fail, ['overloadedrecfldsfail04', ''])
test('overloadedrecfldsfail05', normal, compile_fail, [''])
test('overloadedrecfldsfail06',
     extra_clean(['OverloadedRecFldsFail06_A.hi', 'OverloadedRecFldsFail06_A.o']),
     multimod_compile_fail, ['overloadedrecfldsfail06', ''])
test('overloadedrecfldsfail07', normal, compile_fail, [''])
test('overloadedrecfldsfail08', normal, compile_fail, [''])
test('overloadedrecfldsfail09', normal, compile_fail, [''])
test('overloadedrecfldsfail10',
     extra_clean([ 'OverloadedRecFldsFail10_A.hi', 'OverloadedRecFldsFail10_A.o'
                 , 'OverloadedRecFldsFail10_B.hi', 'OverloadedRecFldsFail10_B.o'
                 , 'OverloadedRecFldsFail10_C.hi', 'OverloadedRecFldsFail10_C.o']),
     multimod_compile_fail, ['overloadedrecfldsfail10', ''])
test('overloadedrecfldsfail11',
     extra_clean(['OverloadedRecFldsFail11_A.hi', 'OverloadedRecFldsFail11_A.o']),
     multimod_compile_fail, ['overloadedrecfldsfail11', ''])
test('overloadedrecfldsfail12',
     extra_clean(['OverloadedRecFldsFail12_A.hi', 'OverloadedRecFldsFail12_A.o']),
     multimod_compile_fail, ['overloadedrecfldsfail12', ''])
test('overloadedrecfldsfail13', normal, compile_fail, [''])
test('overloadedrecfldsfail14', normal, compile_fail, [''])
test('overloadedlabelsfail01', normal, compile_fail, [''])
test('T11167_ambiguous_fixity',
     extra_clean([ 'T11167_ambiguous_fixity_A.hi', 'T11167_ambiguous_fixity_A.o'
                 , 'T11167_ambiguous_fixity_B.hi', 'T11167_ambiguous_fixity_B.o' ]),
     multimod_compile_fail, ['T11167_ambiguous_fixity', ''])
