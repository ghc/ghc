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
