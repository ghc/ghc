test('bug1465', [extra_files(['B1.hs', 'B2.hs', 'C.hs', 'v1/', 'v2/']), js_broken(22349)], makefile_test, [])
