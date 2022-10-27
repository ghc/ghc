test('seward-space-leak', [extra_files(['Main.lhs'])
                          , when(fast(), skip)
                          , js_broken(22352)
                          ], multimod_compile_and_run,
     ['Main', ''])
