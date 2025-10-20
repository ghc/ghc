test('fully-static',
 [ unless(is_musl(), skip)
 , extra_files(['Hello.hs', 'test/'])
 ],
 makefile_test,
 [])
