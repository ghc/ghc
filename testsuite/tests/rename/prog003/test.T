test('rename.prog003', [when(opsys("OSJavaScript"), skip), extra_files(['A.hs', 'B.hs'])], multimod_compile_fail, ['B', '-v0'])
