setTestOpts(extra_run_opts('-ignore-dot-ghci'))

test('break022', extra_clean(['A.hs']), ghci_script, ['break022.script'])
