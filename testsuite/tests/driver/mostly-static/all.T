test('mostly-static', [when(arch('wasm32') or arch('javascript'), skip), extra_files(['Hello.hs', 'test/'])], makefile_test, [])
