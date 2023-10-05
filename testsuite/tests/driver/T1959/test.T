test('T1959', [extra_files(['B.hs', 'C.hs', 'D.hs', 'E1.hs', 'E2.hs']), js_broken(22374)], makefile_test, ['dotest'])
