test('BaLitCmplA', [extra_files(['BaLitCmplTH.hs'])], multimod_compile, ['BaLitCmplA', '-v0'])
test('BaLitRunA', [extra_files(['BaLitCmplTH.hs'])], multimod_compile_and_run, ['BaLitRunA',''])
test('BaLitRunB', [extra_files(['BaLitCmplTH.hs'])], multimod_compile_and_run, ['BaLitRunB',''])
