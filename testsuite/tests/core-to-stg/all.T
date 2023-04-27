# Tests for CorePrep and CoreToStg

test('T19700', normal, compile, ['-O'])
test('T23270', [grep_errmsg(r'patError')], compile, ['-O0 -dsuppress-uniques -ddump-prep'])
