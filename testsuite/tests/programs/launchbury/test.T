
test('launchbury',
     [when(fast(), skip), extra_clean(['Main.hi', 'Main.o'])],
     multimod_compile_and_run,
     ['Main', ''])

