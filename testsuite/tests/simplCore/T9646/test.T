
test('T9646', [extra_files(['Main.hs', 'Natural.hs', 'StrictPrim.hs', 'Type.hs']),
               when(fast(), skip)],
     multimod_compile_and_run, ['Main -ddump-simpl -ddump-to-file', '-XGHC2021'])
