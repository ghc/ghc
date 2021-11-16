test('rename.prog003', [javascript_failures, extra_files(['A.hs', 'B.hs'])], multimod_compile_fail, ['B', '-v0'])
