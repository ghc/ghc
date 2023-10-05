
test('10queens', [extra_files(['Main.hs']),
                  when(fast(), skip)], multimod_compile_and_run, ['Main', ''])
