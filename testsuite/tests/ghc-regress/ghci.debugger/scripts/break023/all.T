setTestOpts(extra_run_opts('-ignore-dot-ghci'), if_compiler_profiled(skip))

test('break023', normal, ghci_script, ['break023.script'])
