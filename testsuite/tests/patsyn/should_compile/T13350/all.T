# Test that importing COMPLETE sets from external packages works

test('T13350',
     [extra_files(['T13350.hs', 'boolean']),
     js_broken(22349)],
     makefile_test, ['T13350'])
