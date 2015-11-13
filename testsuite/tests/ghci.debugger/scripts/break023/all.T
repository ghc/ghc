setTestOpts(extra_run_opts('-ignore-dot-ghci'))

test('break023', extra_clean(['A.hs']), ghci_script, ['break023.script'])
