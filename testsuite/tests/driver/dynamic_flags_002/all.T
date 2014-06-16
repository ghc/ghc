test('dynamic_flags_002A',
     when(fast(), skip), multimod_compile, ['A_Main', '-v0'])
test('dynamic_flags_002B',
     when(fast(), skip), multimod_compile, ['B_Main', '-v0'])
test('dynamic_flags_002C',
     when(fast(), skip), multimod_compile, ['C_Main', '-v0'])
test('dynamic_flags_002D',
     when(fast(), skip), multimod_compile, ['D_Main', '-v0'])
test('dynamic_flags_002Many',
     when(fast(), skip), multimod_compile,
     ['ManyFirst ManySecond ManyThird', '-v0'])

