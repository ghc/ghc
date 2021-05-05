test('WorphansData', [extra_files(['WorphansDataDef.hs'])], multimod_compile, ['WorphansData', '-v0 -fwarn-unused-imports'])
