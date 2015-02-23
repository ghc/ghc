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
