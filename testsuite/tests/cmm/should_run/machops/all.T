setTestOpts(extra_files(['TestMachOp.hs']))

def cmm_test(name):
    test(name, normal, multi_compile_and_run,
         ['TestMachOp', [(name+'.cmm', '')], ''])

cmm_test('T20626a')
cmm_test('T20626b')
