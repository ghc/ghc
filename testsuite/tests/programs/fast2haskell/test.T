
test('fast2haskell', [when(fast(), skip)], multimod_compile_and_run,
     ['Main', ''])
