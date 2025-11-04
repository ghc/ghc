# skip on darwin because the leading underscores will make the test fail
test('T25240', [when(leading_underscore(),skip), fragile(0), req_interp, extra_files(['T25240a.hs'])],
    makefile_test, ['T25240'])
