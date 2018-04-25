setTestOpts(extra_run_opts('-ignore-dot-ghci'))

test('break022', [extra_files(['A1.hs', 'B.hs', 'B.hs-boot', 'C.hs'])], ghci_script, ['break022.script'])
