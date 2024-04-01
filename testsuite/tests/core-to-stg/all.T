# Tests for CorePrep and CoreToStg

test('T19700', normal, compile, ['-O'])
test('T23270', [grep_errmsg(r'patError')], compile, ['-O0 -dsuppress-uniques -ddump-prep'])
test('T23914', normal, compile, ['-O'])
test('T14895', normal, compile, ['-O -ddump-stg-final -dno-typeable-binds -dsuppress-uniques'])
test('T24463', normal, compile, ['-O'])
