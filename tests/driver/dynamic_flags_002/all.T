test('dynamic_flags_002A',
     [extra_files(['A_First.hs', 'A_Main.hs', 'A_Second.hs']),
      when(fast(), skip)], multimod_compile, ['A_Main', '-v0'])
test('dynamic_flags_002B',
     [extra_files(['B_First.hs', 'B_Main.hs', 'B_Second.hs']),
      when(fast(), skip)], multimod_compile, ['B_Main', '-v0'])
test('dynamic_flags_002C',
     [extra_files(['C_Child.hs', 'C_Main.hs']),
      when(fast(), skip)], multimod_compile, ['C_Main', '-v0'])
test('dynamic_flags_002D',
     [extra_files(['D_Child.hs', 'D_Main.hs']),
      when(fast(), skip)], multimod_compile, ['D_Main', '-v0'])
test('dynamic_flags_002Many',
     [extra_files(['ManyFirst.hs', 'ManySecond.hs', 'ManyThird.hs']),
      when(fast(), skip)], multimod_compile,
     ['ManyFirst ManySecond ManyThird', '-v0'])

