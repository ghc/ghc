test('fully-static',
 [ unless(have_fully_static_linking(), skip)
 , extra_files(['Hello.hs', 'test/'])
 ],
 makefile_test,
 [])
