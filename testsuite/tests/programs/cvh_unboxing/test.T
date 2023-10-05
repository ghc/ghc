test('cvh_unboxing', [extra_files(['Append.lhs', 'Main.lhs', 'Types.lhs']),
                      when(fast(), skip)], multimod_compile_and_run,
     ['Main', ''])
