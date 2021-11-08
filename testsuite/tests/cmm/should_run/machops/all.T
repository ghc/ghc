setTestOpts(extra_files(['TestMachOp.hs']))

def cmm_test(name):
    test(name, normal, multi_compile_and_run,
         ['TestMachOp', [(name+'.cmm', '')], ''])

cmm_test('T20626a')
cmm_test('T20626b')
cmm_test('T20637')
cmm_test('T20638')
cmm_test('T20634')
