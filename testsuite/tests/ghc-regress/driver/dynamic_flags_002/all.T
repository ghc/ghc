test('dynamic_flags_002A', skip_if_fast, multimod_compile, ['A_Main', '-v0'])
test('dynamic_flags_002B', skip_if_fast, multimod_compile, ['B_Main', '-v0'])
test('dynamic_flags_002C', skip_if_fast, multimod_compile, ['C_Main', '-v0'])
test('dynamic_flags_002D', skip_if_fast, multimod_compile, ['D_Main', '-v0'])
test('dynamic_flags_002Many', skip_if_fast, multimod_compile, ['ManyFirst ManySecond ManyThird', '-v0'])

