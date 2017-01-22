setTestOpts(extra_run_opts('-ignore-dot-ghci'))

test('break023', [], ghci_script, ['break023.script'])
