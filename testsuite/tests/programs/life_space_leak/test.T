test('life_space_leak', [when(fast(), skip)], multimod_compile_and_run,
     ['Main', ''])
