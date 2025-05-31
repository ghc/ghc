setTestOpts([omit_ghci, when(fast(), skip), js_skip])

test('recsel',
     [ignore_extension,
      when(arch('wasm32'), fragile(23243))],
     compile_and_run, ['-fhpc'])

