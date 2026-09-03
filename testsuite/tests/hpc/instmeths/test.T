setTestOpts([omit_ghci, when(fast(), skip), js_skip])

test('instmeths', normal, compile_and_run, ['-fhpc'])
