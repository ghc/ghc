test('seward-space-leak', [extra_files(['Main.lhs'])
                          , when(fast(), skip)
                          , js_skip
                          ], multimod_compile_and_run,
     ['Main', ''])
