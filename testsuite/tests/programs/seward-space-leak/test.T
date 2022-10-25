test('seward-space-leak', [extra_files(['Main.lhs'])
                          , when(fast(), skip)
                          , req_js_compactor
                          ], multimod_compile_and_run,
     ['Main', ''])
