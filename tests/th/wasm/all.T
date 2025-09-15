setTestOpts([
  unless(arch('wasm32'), skip)
])

test('T25473', [], multimod_compile, ['T25473B', '-v0'])
