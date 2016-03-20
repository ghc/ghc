
test('T9646',
     [when(fast(), skip), extra_clean(['Main.hi', 'Main.o'])],
     multimod_compile_and_run,
     ['Main -ddump-simpl -ddump-to-file', ''])

