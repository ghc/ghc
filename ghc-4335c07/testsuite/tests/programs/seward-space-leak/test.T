test('seward-space-leak', [extra_files(['Main.lhs']),
                           when(fast(), skip)], multimod_compile_and_run,
     ['Main', ''])
