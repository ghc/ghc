test('cholewo-eval', [extra_files(['Arr.lhs', 'Main.lhs']),
                      when(fast(), skip)], multimod_compile_and_run,
     ['Main', ''])
