setTestOpts(extra_run_opts('-ignore-dot-ghci'))

test('break023', [extra_files(['A1.hs', 'B.hs', 'B.hs-boot', 'C.hs'])], ghci_script, ['break023.script'])
