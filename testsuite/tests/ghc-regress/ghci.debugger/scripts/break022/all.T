setTestOpts(extra_run_opts('-ignore-dot-ghci'))

test('break022', normal, ghci_script, ['break022.script'])
