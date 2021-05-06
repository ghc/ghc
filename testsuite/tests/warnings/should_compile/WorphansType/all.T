test('WorphansType', [extra_files(['WorphansTypeDef.hs'])], multimod_compile, ['WorphansType', '-v0 -Worphans-type'])
