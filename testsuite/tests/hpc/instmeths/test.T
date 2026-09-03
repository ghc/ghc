setTestOpts([omit_ghci, when(fast(), skip), js_skip])

test('instmeths',
     [ignore_extension],
     compile_and_run, ['-fhpc'])
