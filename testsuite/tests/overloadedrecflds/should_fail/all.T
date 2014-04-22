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
test('overloadedrecfldsfail07', skip, compile_fail, [''])
test('overloadedrecfldsfail08',
     extra_clean(['OverloadedRecFldsFail08_A.hi', 'OverloadedRecFldsFail08_A.o']),
     multimod_compile_fail, ['overloadedrecfldsfail08', ''])
test('overloadedrecfldsfail09', normal, compile_fail, [''])
test('overloadedrecfldsfail10', normal, compile_fail, [''])
