
test('T9646', [when(fast(), skip), expect_broken_for(9646, ['ghci'])],
     multimod_compile_and_run, ['Main -ddump-simpl -ddump-to-file', ''])
