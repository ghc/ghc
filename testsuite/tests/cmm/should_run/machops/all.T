setTestOpts(extra_files(['TestMachOp.hs']))

def cmm_test(name, ws):
    test(name,
         [unless(wordsize(ws), skip), normal],
         multi_compile_and_run,
         ['TestMachOp', [(name+'.cmm', '')], ''])

cmm_test('T20626a', ws=64)
cmm_test('T20626b', ws=64)
cmm_test('T20638',  ws=64)
cmm_test('T20634',  ws=64)
