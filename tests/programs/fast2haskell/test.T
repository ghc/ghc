
test('fast2haskell', [extra_files(['Fast2haskell.hs', 'Main.hs']),
                      when(fast(), skip)], multimod_compile_and_run,
     ['Main', ''])
