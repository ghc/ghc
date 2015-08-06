setTestOpts(when(compiler_profiled(), skip))

test('T10549', [], ghci_script, ['T10549.script'])
