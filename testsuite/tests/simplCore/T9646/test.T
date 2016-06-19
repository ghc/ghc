
test('T9646',
     [when(fast(), skip),
      expect_broken_for(9646, ['ghci']),
      extra_clean(['Main.hi', 'Main.o', 'Main.dump-simpl',
                   'Natural.dump-simpl', 'StrictPrim.dump-simpl',
                   'Type.dump-simpl'])],
     multimod_compile_and_run,
     ['Main -ddump-simpl -ddump-to-file', ''])

